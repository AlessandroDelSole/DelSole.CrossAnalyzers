Imports System.Composition
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.CodeRefactorings
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<ExportCodeRefactoringProvider(LanguageNames.VisualBasic, Name:=NameOf(MakeRelayCommandRefactoring)), [Shared]>
Friend Class MakeRelayCommandRefactoring
    Inherits CodeRefactoringProvider

    Public NotOverridable Overrides Async Function _
         ComputeRefactoringsAsync(context As CodeRefactoringContext) As Task
        Dim root = Await context.Document.
          GetSyntaxRootAsync(context.CancellationToken).
          ConfigureAwait(False)
        ' Find the node at the selection.
        Dim node = root.FindNode(context.Span)
        ' Only offer a refactoring if the selected node is a class statement node.
        Dim classDecl = TryCast(node, ClassStatementSyntax)
        If classDecl Is Nothing Then Return
        Dim action = CodeAction.Create("Make RelayCommand(Of T) class",
                                       Function(c) _
                                       MakeRelayCommandAsync(context.Document,
                                                             classDecl, c))
        ' Register this code action.
        context.RegisterRefactoring(action)
    End Function
    Private Async Function MakeRelayCommandAsync(document As Document,
     classDeclaration As ClassStatementSyntax, cancellationToken As CancellationToken) _
     As Task(Of Document)
        ' The class definition represented as source text
        Dim newImplementation = "Class RelayCommand(Of T)
   Implements ICommand
   Private ReadOnly _execute As Action(Of T)
   Private ReadOnly _canExecute As Predicate(Of T)
   Public Sub New(ByVal execute As Action(Of T))
     Me.New(execute, Nothing)
   End Sub
   Public Sub New(ByVal execute As Action(Of T), ByVal canExecute As Predicate(Of T))
     If execute Is Nothing Then
       Throw New ArgumentNullException(""execute"")
     End If
     _execute = execute
     _canExecute = canExecute
   End Sub
   <DebuggerStepThrough> _
   Public Function CanExecute(ByVal parameter As Object) As Boolean Implements ICommand.CanExecute
     Return If(_canExecute Is Nothing, True, _canExecute(CType(parameter, T)))
   End Function
   Public Event CanExecuteChanged As EventHandler Implements ICommand.CanExecuteChanged
   Public Sub RaiseCanExecuteChanged()
     RaiseEvent CanExecuteChanged(Me, EventArgs.Empty)
   End Sub
   Public Sub Execute(ByVal parameter As Object) Implements ICommand.Execute
     _execute(CType(parameter, T))
   End Sub
 End Class"
        ' 1. ParseSyntaxTree() gets a new SyntaxTree from the source text
        ' 2. GetRoot() gets the root node of the tree
        ' 3. OfType(Of ClassDeclarationSyntax)().FirstOrDefault()
        '    retrieves the only class definition in the tree
        ' 4. WithAdditionalAnnotations() Is invoked for code formatting
        Dim newClassNode = SyntaxFactory.ParseSyntaxTree(newImplementation).
        GetRoot().DescendantNodes().
          OfType(Of ClassBlockSyntax)().
          FirstOrDefault().
          WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation)
        Dim parentBlock = CType(classDeclaration.Parent, ClassBlockSyntax)
        Dim root = Await document.GetSyntaxRootAsync(cancellationToken)
        ' Generate a new CompilationUnitSyntax (which represents a code file)
        ' replacing the old class with the new one
        Dim newRoot As CompilationUnitSyntax =
          root.ReplaceNode(parentBlock, newClassNode)
        'Detect if an Imports System.Windows.Input directive already exists
        If Not newRoot.Imports.Any(Function(i) i.ToFullString.
          Contains("System.Windows.Input")) Then
            'If not, add one
            Dim newImp = SyntaxFactory.
              ImportsStatement(SyntaxFactory.
              SingletonSeparatedList(Of ImportsClauseSyntax) _
              (SyntaxFactory.
              SimpleImportsClause(SyntaxFactory.
              ParseName("System.Windows.Input"))))
            newRoot = newRoot.AddImports(newImp)
        End If
        Dim newDocument = document.WithSyntaxRoot(newRoot)
        Return newDocument
    End Function

End Class