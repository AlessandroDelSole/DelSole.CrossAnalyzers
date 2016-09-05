<DiagnosticAnalyzer(LanguageNames.VisualBasic)>
Public Class DelSoleCrossAnalyzersVisualBasicAnalyzer
    Inherits DiagnosticAnalyzer

    Public Const DiagnosticId As String = "CRA012"
    Friend Shared ReadOnly Title As String = "List(Of T) is improper for data-binding"
    Friend Shared ReadOnly MessageFormat As String = "'{0}' is of type List(Of T). Consider assigning an object of type ObservableCollection(Of T) instead."
    Friend Shared ReadOnly Category As String = "Platform"

    Private Shared Rule As New DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault:=True,
                                                    helpLinkUri:="https://github.com/AlessandroDelSole/DelSole.CrossAnalyzers/wiki")

    Public Overrides ReadOnly Property SupportedDiagnostics As ImmutableArray(Of DiagnosticDescriptor)
        Get
            Return ImmutableArray.Create(Rule)
        End Get
    End Property

    Public Overrides Sub Initialize(context As AnalysisContext)
        ' TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
        ' See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
        context.RegisterSyntaxNodeAction(AddressOf AnalyzeAssignementStatements, SyntaxKind.SimpleAssignmentStatement)
    End Sub

    Private Sub AnalyzeSymbol(context As SymbolAnalysisContext)
        ' TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find

        Dim namedTypeSymbol = CType(context.Symbol, INamedTypeSymbol)

        ' Find just those named type symbols with names containing lowercase letters.
        If namedTypeSymbol.Name.ToCharArray.Any(AddressOf Char.IsLower) Then
            ' For all such symbols, produce a diagnostic.
            Dim diag = Diagnostic.Create(Rule, namedTypeSymbol.Locations(0), namedTypeSymbol.Name)

            context.ReportDiagnostic(diag)
        End If
    End Sub

    Public Sub AnalyzeAssignementStatements(context As SyntaxNodeAnalysisContext)
        Dim node = TryCast(context.Node, AssignmentStatementSyntax)
        If node Is Nothing Then
            Return
        End If

        Dim leftPart = node.Left.ToFullString
        If leftPart.Contains("ItemsSource") = False Then
            If leftPart.Contains("DataContext") = False Then
                Return
            End If
        End If

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

        If Not returnType.ToLowerInvariant().Contains("system.collections.generic.list") Then Return

        Dim diagn = Diagnostic.Create(Rule, node.Right.GetLocation(), node.Right.ToString())
        context.ReportDiagnostic(diagn)
    End Sub
End Class