Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class AsyncVoidReturnAnalyzer
    Inherits DiagnosticAnalyzer
    Public Const DiagnosticId = "CRA011"
    Friend Shared ReadOnly Title As LocalizableString = "Avoid Async Sub methods if they are not event handlers. Use Async Function"
    Friend Shared ReadOnly MessageFormat As LocalizableString = "Asynchronous method '{0}' does not return any value but it's not an event handler"
    Friend Const Category = "Naming"

    Friend Shared Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, True,
                                                   helpLinkUri:="https://github.com/AlessandroDelSole/DelSole.CrossAnalyzers/wiki")

    Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
        Get
            Return ImmutableArray.Create(Rule)
        End Get
    End Property


    Public Overrides Sub Initialize(context As AnalysisContext)
        ' TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
        context.RegisterSymbolAction(AddressOf AnalyzeSymbol, SymbolKind.Method)
    End Sub

    Private Sub AnalyzeSymbol(context As SymbolAnalysisContext)
        ' TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find

        Dim methodSymbol = CType(context.Symbol, IMethodSymbol)

        If methodSymbol.IsAsync = False Then
            Return
        End If

        If methodSymbol.HandledEvents.Any Then
            Return
        End If

        If methodSymbol.ReturnsVoid Then
            Dim diag = Diagnostic.Create(Rule, methodSymbol.Locations(0), methodSymbol.Name)

            context.ReportDiagnostic(diag)
        End If
    End Sub
End Class