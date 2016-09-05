Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class AsyncSuffixAnalyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "CRA010"
    Friend Shared ReadOnly Title As LocalizableString = "Names of asynchronous methods should end with Async"
    Friend Shared ReadOnly MessageFormat As LocalizableString = "Name of asynchronous method '{0}' does not end with Async"
    Friend Const Category = "Naming"

    Friend Shared Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, True,
                                                   helpLinkUri:="https://github.com/AlessandroDelSole/DotNetAnalyzers/wiki/DNA-200---Names-of-asynchronous-methods-should-end-with-Async")

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

        If Not methodSymbol.Name.ToLowerInvariant.EndsWith("async") Then
            Dim diag = Diagnostic.Create(Rule, methodSymbol.Locations(0), methodSymbol.Name)

            context.ReportDiagnostic(diag)
        End If
    End Sub
End Class