Imports System.Composition
Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.CodeFixes
Imports Microsoft.CodeAnalysis.CodeRefactorings
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<ExportCodeFixProvider(LanguageNames.VisualBasic, Name:=NameOf(AsyncVoidReturnCodeFix)), [Shared]>
Public Class AsyncVoidReturnCodeFix
    Inherits CodeFixProvider

    Public Const DiagnosticId As String = AsyncVoidReturnAnalyzer.DiagnosticId
    Private Const title As String = "Change Async Sub to Async Function"

    Public NotOverridable Overrides ReadOnly Property FixableDiagnosticIds As ImmutableArray(Of String)
        Get
            Return ImmutableArray.Create(DiagnosticId)
        End Get
    End Property

    Public NotOverridable Overrides Function GetFixAllProvider() As FixAllProvider
        Return WellKnownFixAllProviders.BatchFixer
    End Function

    Public NotOverridable Overrides Async Function RegisterCodeFixesAsync(context As CodeFixContext) As Task
        Dim root = Await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(False)

        ' TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest

        Dim diagnostic = context.Diagnostics.First()
        Dim diagnosticSpan = diagnostic.Location.SourceSpan

        ' Find the type statement identified by the diagnostic.
        Dim declaration = root.FindNode(context.Span)

        ' Register a code action that will invoke the fix.
        context.RegisterCodeFix(
            CodeAction.Create(
                title:=title,
                createChangedDocument:=Function(c) ChangeAsyncSubToFunctionAsync(context.Document, declaration, c),
                equivalenceKey:=title),
            diagnostic)
    End Function

    Private Async Function ChangeAsyncSubToFunctionAsync(document As Document,
                                                         methodStmt As MethodStatementSyntax,
                                                         cancellationToken As CancellationToken) _
                                                         As Task(Of Document)

        Dim root = Await document.GetSyntaxRootAsync

        Dim parentBlock = DirectCast(methodStmt.Parent, MethodBlockSyntax)

        Dim newMethodSyntax = SyntaxFactory.FunctionStatement(methodStmt.Identifier).
            WithAsClause(SyntaxFactory.SimpleAsClause(SyntaxFactory.ParseTypeName("System.Threading.Tasks.Task"))).
            WithAttributeLists(methodStmt.AttributeLists).
            WithHandlesClause(methodStmt.HandlesClause).WithImplementsClause(methodStmt.ImplementsClause).
            WithModifiers(methodStmt.Modifiers).WithParameterList(methodStmt.ParameterList).
            WithTypeParameterList(methodStmt.TypeParameterList).WithTrailingTrivia(methodStmt.GetTrailingTrivia).
            WithLeadingTrivia(methodStmt.GetLeadingTrivia).NormalizeWhitespace


        Dim newMethodBlock = parentBlock.Update(SyntaxKind.FunctionBlock, newMethodSyntax, parentBlock.Statements, SyntaxFactory.EndFunctionStatement).NormalizeWhitespace.
            WithLeadingTrivia(parentBlock.GetLeadingTrivia).WithTrailingTrivia(parentBlock.GetTrailingTrivia)

        Dim newRoot = root.ReplaceNode(parentBlock, newMethodBlock)

        Dim newDocument = document.WithSyntaxRoot(newRoot)
        Return newDocument
    End Function
End Class