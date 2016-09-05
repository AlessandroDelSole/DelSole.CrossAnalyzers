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

<ExportCodeRefactoringProvider(LanguageNames.VisualBasic, Name:=NameOf(MakeViewModelBaseRefactoring)), [Shared]>
Public Class MakeViewModelBaseRefactoring
    Inherits CodeRefactoringProvider

    Private Title As String = "Make ViewModelBase class"
    Public NotOverridable Overrides Async Function _
   ComputeRefactoringsAsync(context As CodeRefactoringContext) As Task
        ' Get the root node of the syntax tree
        Dim root = Await context.Document.
     GetSyntaxRootAsync(context.CancellationToken).
     ConfigureAwait(False)
        ' Find the node at the selection.
        Dim node = root.FindNode(context.Span)
        ' Is this a class statement node?
        Dim classDecl = TryCast(node, ClassStatementSyntax)
        If classDecl Is Nothing Then Return
        ' If so, create an action to offer a refactoring
        Dim action = CodeAction.Create(title:=Title,
                                  createChangedDocument:=Function(c) _
                                  MakeViewModelBaseAsync(context.
                                  Document, classDecl, c),
                                  equivalenceKey:=Title)
        ' Register this code action.
        context.RegisterRefactoring(action)
    End Function

    Private Async Function MakeViewModelBaseAsync(document As Document,
   classDeclaration As ClassStatementSyntax,
   cancellationToken As CancellationToken) As Task(Of Document)
        ' The class definition represented as source text
        Dim newImplementation = "Public MustInherit Class ViewModelBase
 Implements INotifyPropertyChanged
 Public Event PropertyChanged(sender As Object,
                              e As PropertyChangedEventArgs) _
                              Implements INotifyPropertyChanged.PropertyChanged
 Protected Sub OnPropertyChanged(propertyName As String)
   RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(propertyName))
 End Sub
 End Class
 "
        ' 1. ParseSyntaxTree() gets a New SyntaxTree from the source text
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
        ' Get the root SyntaxNode of the document
        Dim root = Await document.GetSyntaxRootAsync(cancellationToken)
        ' Generate a new CompilationUnitSyntax (which represents a code file)
        ' replacing the old class with the new one
        Dim newRoot As CompilationUnitSyntax = root.ReplaceNode(parentBlock,
                                                           newClassNode).
                                                           NormalizeWhitespace()
        ' Detect if an Imports System.ComponentModel directive already exists
        If Not newRoot.Imports.Any(Function(i) i.ImportsClauses.
        Where(Function(f) f.ToString = "System.ComponentModel").Any) Then
            ' If not, add one
            Dim newImp = SyntaxFactory.
       ImportsStatement(SyntaxFactory.
       SingletonSeparatedList(Of ImportsClauseSyntax)(SyntaxFactory.
       SimpleImportsClause(SyntaxFactory.
       ParseName("System.ComponentModel"))))
            newRoot = newRoot.AddImports(newImp)
        End If
        ' Generate a new document based on the new SyntaxNode
        Dim newDocument = document.WithSyntaxRoot(newRoot)
        ' Return the new document
        Return newDocument
    End Function

End Class