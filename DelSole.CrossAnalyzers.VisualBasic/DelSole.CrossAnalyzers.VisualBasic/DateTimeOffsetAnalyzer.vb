Imports System.Collections.Immutable
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class DateTimeOffsetAnalyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId = "CRA013"
    Friend Shared ReadOnly Title As LocalizableString = "Assigning DateTime to objects of type DateTimeOffset might result in runtime errors"
    Friend Shared ReadOnly MessageFormat As LocalizableString = "'{0}' should be of type DateTimeOffset"
    Friend Const Category = "Platform"

    Friend Shared Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, True, helpLinkUri:="https://github.com/AlessandroDelSole/DelSole.CrossAnalyzers/wiki")

    Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
        Get
            Return ImmutableArray.Create(Rule)
        End Get
    End Property

    Public Overrides Sub Initialize(context As AnalysisContext)
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeAssignementStatements, SyntaxKind.SimpleAssignmentStatement)
    End Sub

    Public Sub AnalyzeAssignementStatements(context As SyntaxNodeAnalysisContext)
        Dim node = TryCast(context.Node, AssignmentStatementSyntax)
        If node Is Nothing Then
            Return
        End If


        Dim leftSymbolInfo = context.SemanticModel.GetSymbolInfo(node.Left).Symbol

        If leftSymbolInfo.Kind = SymbolKind.Local Then Return

        Dim tempSymbol As Boolean

        If leftSymbolInfo.Kind = SymbolKind.Property Then
            tempSymbol = CType(leftSymbolInfo, IPropertySymbol).Type.ToString().Contains("DateTimeOffset")

        Else
            tempSymbol = CType(leftSymbolInfo, IFieldSymbol).Type.ToString().Contains("DateTimeOffset")
        End If

        If tempSymbol = False Then Return

        Dim returnType = ""
        Dim SymbolInfo = context.SemanticModel.GetSymbolInfo(node.Right).Symbol
        If TypeOf (SymbolInfo) Is ILocalSymbol Then
            returnType = CType(SymbolInfo, ILocalSymbol).Type.ToString()
        ElseIf TypeOf (SymbolInfo) Is IPropertySymbol Then
            returnType = CType(SymbolInfo, IPropertySymbol).Type.ToString()
        ElseIf TypeOf (SymbolInfo) Is IFieldSymbol Then
            returnType = CType(SymbolInfo, IFieldSymbol).Type.ToString()
        ElseIf TypeOf (SymbolInfo) Is IMethodSymbol Then
            returnType = CType(SymbolInfo, IMethodSymbol).ReturnType.ToString()
        Else
            Return
        End If

        If Not returnType.ToLowerInvariant() = "date" Then Return

        Dim diagn = Diagnostic.Create(Rule, node.Right.GetLocation(), node.Right.ToString())
        context.ReportDiagnostic(diagn)
    End Sub
End Class