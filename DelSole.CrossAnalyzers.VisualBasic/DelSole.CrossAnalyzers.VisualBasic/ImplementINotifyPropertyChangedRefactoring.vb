Imports System.Composition
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CodeActions
Imports Microsoft.CodeAnalysis.CodeRefactorings
Imports Microsoft.CodeAnalysis.Editing
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Rename
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

' This refactoring is a remake of an example created by the Roslyn team, available at: https://github.com/dotnet/roslyn/tree/master/src/Samples/VisualBasic/ImplementNotifyPropertyChanged
' The original sample has a different implementation of the interface, here I provide a more common implementation.

<ExportCodeRefactoringProvider(LanguageNames.VisualBasic, Name:=NameOf(ImplementINotifyPropertyChangedRefactoring)), [Shared]>
Public Class ImplementINotifyPropertyChangedRefactoring
    Inherits CodeRefactoringProvider

    Public NotOverridable Overrides Async Function ComputeRefactoringsAsync(context As CodeRefactoringContext) As Task
        Dim document = context.Document
        Dim textSpan = context.Span
        Dim cancellationToken = context.CancellationToken

        Dim root = DirectCast(Await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(False), CompilationUnitSyntax)
        Dim model = Await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(False)

        ' if length Is 0 then no particular range Is selected, so pick the first enclosing declaration
        If textSpan.Length = 0 Then
            Dim decl = root.FindToken(textSpan.Start).Parent.AncestorsAndSelf().OfType(Of DeclarationStatementSyntax)().FirstOrDefault()
            If decl IsNot Nothing Then
                textSpan = decl.Span
            End If
        End If

        Dim properties = ExpansionChecker.GetExpandableProperties(textSpan, root, model)

        If properties.Any Then
#Disable Warning RS0005
            context.RegisterRefactoring(
                CodeAction.Create("Implement INotifyPropertyChanged interface",
                                  Function(c) ImplementNotifyPropertyChangedAsync(document, root, model, properties, c)))
#Enable Warning RS0005
        End If
    End Function

    Private Async Function ImplementNotifyPropertyChangedAsync(document As Document, root As CompilationUnitSyntax, model As SemanticModel, properties As IEnumerable(Of ExpandablePropertyInfo), cancellationToken As CancellationToken) As Task(Of Document)
        document = document.WithSyntaxRoot(CodeGeneration.ImplementINotifyPropertyChanged(root, model, properties, document.Project.Solution.Workspace))
        document = Await Simplifier.ReduceAsync(document, Simplifier.Annotation, cancellationToken:=cancellationToken).ConfigureAwait(False)
        document = Await Formatter.FormatAsync(document, Formatter.Annotation, cancellationToken:=cancellationToken).ConfigureAwait(False)
        Return document
    End Function
End Class

Friend Class ExpansionChecker
    Friend Shared Function GetExpandableProperties(span As TextSpan, root As SyntaxNode, model As SemanticModel) As IEnumerable(Of ExpandablePropertyInfo)
        Dim propertiesInTypes = root.DescendantNodes(span) _
            .OfType(Of PropertyStatementSyntax) _
            .Select(Function(p) GetExpandablePropertyInfo(p, model)) _
            .Where(Function(p) p IsNot Nothing) _
            .GroupBy(Function(p) p.PropertyDeclaration.FirstAncestorOrSelf(Of TypeBlockSyntax))

        Return If(propertiesInTypes.Any(),
            propertiesInTypes.First(),
            Enumerable.Empty(Of ExpandablePropertyInfo))
    End Function

    ''' <summary> Returns true if the specified <see cref="PropertyBlockSyntax"/>  can be expanded to
    ''' include support for <see cref="INotifyPropertyChanged"/> . </summary>
    Friend Shared Function GetExpandablePropertyInfo(propertyStatement As PropertyStatementSyntax, model As SemanticModel) As ExpandablePropertyInfo
        If propertyStatement.ContainsDiagnostics Then
            Return Nothing
        End If

        If propertyStatement.Modifiers.Any(SyntaxKind.SharedKeyword) OrElse
           propertyStatement.Modifiers.Any(SyntaxKind.MustOverrideKeyword) Then
            Return Nothing
        End If

        If propertyStatement.AsClause Is Nothing Then
            Return Nothing
        End If

        Dim propertyBlock = TryCast(propertyStatement.Parent, PropertyBlockSyntax)
        If propertyBlock Is Nothing Then
            ' We're an auto property, we can be expanded.
            Return New ExpandablePropertyInfo With
            {
                .PropertyDeclaration = propertyStatement,
                .BackingFieldName = GenerateFieldName(propertyStatement, model),
                .NeedsBackingField = True,
                .Type = model.GetDeclaredSymbol(propertyStatement).Type
            }
        End If

        ' Not an auto property, look more closely.
        If propertyBlock.ContainsDiagnostics Then
            Return Nothing
        End If

        ' Only expand properties with both a getter and a setter.
        Dim getter As AccessorBlockSyntax = Nothing
        Dim setter As AccessorBlockSyntax = Nothing
        If Not TryGetAccessors(propertyBlock, getter, setter) Then
            Return Nothing
        End If

        Dim backingField As IFieldSymbol = Nothing
        Return If(IsExpandableGetter(getter, model, backingField) AndAlso IsExpandableSetter(setter, model, backingField),
            New ExpandablePropertyInfo With {.PropertyDeclaration = propertyBlock, .BackingFieldName = backingField.Name},
            Nothing)
    End Function

    ''' <summary> Retrieves the get and set accessor declarations of the specified property.
    ''' Returns true if both get and set accessors exist; otherwise false. </summary>
    Friend Shared Function TryGetAccessors(propertyBlock As PropertyBlockSyntax,
                                    ByRef getter As AccessorBlockSyntax,
                                    ByRef setter As AccessorBlockSyntax) As Boolean
        Dim accessors = propertyBlock.Accessors
        getter = accessors.FirstOrDefault(Function(ad) ad.AccessorStatement.Kind() = SyntaxKind.GetAccessorStatement)
        setter = accessors.FirstOrDefault(Function(ad) ad.AccessorStatement.Kind() = SyntaxKind.SetAccessorStatement)
        Return getter IsNot Nothing AndAlso setter IsNot Nothing
    End Function

    Private Shared Function IsExpandableGetter(getter As AccessorBlockSyntax,
                                       semanticModel As SemanticModel,
                                       ByRef backingField As IFieldSymbol) As Boolean
        backingField = GetBackingFieldFromGetter(getter, semanticModel)
        Return backingField IsNot Nothing
    End Function

    Private Shared Function GetBackingFieldFromGetter(getter As AccessorBlockSyntax, semanticModel As SemanticModel) As IFieldSymbol
        If Not getter.Statements.Any() Then
            Return Nothing
        End If

        Dim statements = getter.Statements
        If statements.Count <> 1 Then
            Return Nothing
        End If

        Dim returnStatement = TryCast(statements.Single(), ReturnStatementSyntax)
        If returnStatement Is Nothing OrElse returnStatement.Expression Is Nothing Then
            Return Nothing
        End If

        Return TryCast(semanticModel.GetSymbolInfo(returnStatement.Expression).Symbol, IFieldSymbol)
    End Function

    Private Shared Function GenerateFieldName(propertyStatement As PropertyStatementSyntax, semanticModel As SemanticModel) As String
        Dim baseName = propertyStatement.Identifier.ValueText
        baseName = "_" & Char.ToLower(baseName(0)) & baseName.Substring(1)
        Dim propertySymbol = TryCast(semanticModel.GetDeclaredSymbol(propertyStatement), IPropertySymbol)
        If propertySymbol Is Nothing OrElse propertySymbol.ContainingType Is Nothing Then
            Return baseName
        End If

        Dim index = 0
        Dim name = baseName
        While DirectCast(propertySymbol.ContainingType, INamedTypeSymbol).MemberNames.Contains(name, StringComparer.OrdinalIgnoreCase)
            name = baseName & index.ToString()
            index += 1
        End While

        Return name
    End Function

    Private Shared Function IsExpandableSetter(setter As AccessorBlockSyntax,
                                        semanticModel As SemanticModel,
                                        backingField As IFieldSymbol) As Boolean
        Return IsExpandableSetterPattern1(setter, backingField, semanticModel) OrElse
            IsExpandableSetterPattern2(setter, backingField, semanticModel) OrElse
            IsExpandableSetterPattern3(setter, backingField, semanticModel)
    End Function

    Private Shared Function IsExpandableSetterPattern1(setter As AccessorBlockSyntax,
                                                backingField As IFieldSymbol,
                                                semanticModel As SemanticModel) As Boolean
        Dim statements = setter.Statements
        If statements.Count <> 1 Then
            Return False
        End If

        Dim expressionStatement = statements.First()
        Return IsAssignmentOfPropertyValueParameterToBackingField(expressionStatement, backingField, semanticModel)
    End Function

    Private Shared Function IsExpandableSetterPattern2(setter As AccessorBlockSyntax,
                                                backingField As IFieldSymbol,
                                                semanticModel As SemanticModel) As Boolean
        Dim statements = setter.Statements
        If statements.Count <> 1 Then
            Return False
        End If

        Dim statement As StatementSyntax = Nothing
        Dim condition As ExpressionSyntax = Nothing

        If Not GetConditionAndSingleStatementFromIfStatement(statements(0), statement, condition) Then
            Return False
        End If

        If Not IsAssignmentOfPropertyValueParameterToBackingField(statement, backingField, semanticModel) Then
            Return False
        End If

        If condition Is Nothing OrElse condition.Kind() <> SyntaxKind.NotEqualsExpression Then
            Return False
        End If

        Return ComparesPropertyValueParameterAndBackingField(DirectCast(condition, BinaryExpressionSyntax),
                                                             backingField,
                                                             semanticModel)
    End Function

    Private Shared Function IsExpandableSetterPattern3(setter As AccessorBlockSyntax,
                                                backingField As IFieldSymbol,
                                                semanticModel As SemanticModel) As Boolean
        Dim statements = setter.Statements
        If statements.Count <> 2 Then
            Return False
        End If

        Dim statement As StatementSyntax = Nothing
        Dim condition As ExpressionSyntax = Nothing

        If Not GetConditionAndSingleStatementFromIfStatement(statements(0), statement, condition) Then
            Return False
        End If

        Dim returnStatement = TryCast(statement, ReturnStatementSyntax)
        If returnStatement Is Nothing OrElse returnStatement.Expression IsNot Nothing Then
            Return False
        End If

        If Not IsAssignmentOfPropertyValueParameterToBackingField(statements(1), backingField, semanticModel) Then
            Return False
        End If

        If condition.Kind() <> SyntaxKind.EqualsExpression Then
            Return False
        End If

        Return ComparesPropertyValueParameterAndBackingField(DirectCast(condition, BinaryExpressionSyntax),
                                                             backingField,
                                                             semanticModel)
    End Function

    Private Shared Function IsAssignmentOfPropertyValueParameterToBackingField(statement As StatementSyntax,
                                                                        backingField As IFieldSymbol, semanticModel As SemanticModel) As Boolean
        If statement.Kind() <> SyntaxKind.SimpleAssignmentStatement Then
            Return False
        End If

        Dim assignment = DirectCast(statement, AssignmentStatementSyntax)
        Return IsBackingField(assignment.Left, backingField, semanticModel) AndAlso IsPropertyValueParameter(assignment.Right, semanticModel)
    End Function

    Private Shared Function GetConditionAndSingleStatementFromIfStatement(ifStatement As StatementSyntax,
                                                                   ByRef statement As StatementSyntax,
                                                                   ByRef condition As ExpressionSyntax) As Boolean
        Dim multiLineIfStatement = TryCast(ifStatement, MultiLineIfBlockSyntax)
        If multiLineIfStatement IsNot Nothing Then
            If multiLineIfStatement.Statements.Count <> 1 Then
                Return False
            End If

            statement = multiLineIfStatement.Statements.First()
            condition = multiLineIfStatement.IfStatement.Condition
            Return True
        Else
            Dim singleLineIfStatement = TryCast(ifStatement, SingleLineIfStatementSyntax)
            If singleLineIfStatement IsNot Nothing Then
                If singleLineIfStatement.Statements.Count <> 1 Then
                    Return False
                End If

                statement = singleLineIfStatement.Statements.First()
                condition = singleLineIfStatement.Condition
                Return True
            End If
            Return False
        End If
    End Function

    Private Shared Function IsBackingField(expression As ExpressionSyntax,
                                    backingField As IFieldSymbol,
                                    semanticModel As SemanticModel) As Boolean
        Return Object.Equals(semanticModel.GetSymbolInfo(expression).Symbol, backingField)
    End Function

    Private Shared Function IsPropertyValueParameter(expression As ExpressionSyntax,
                                              semanticModel As SemanticModel) As Boolean
        Dim symbol = semanticModel.GetSymbolInfo(expression).Symbol

        Return symbol IsNot Nothing AndAlso
            symbol.Kind = SymbolKind.Parameter AndAlso
            symbol.ContainingSymbol.Kind = SymbolKind.Method AndAlso
            DirectCast(symbol.ContainingSymbol, IMethodSymbol).MethodKind = MethodKind.PropertySet
    End Function

    Private Shared Function ComparesPropertyValueParameterAndBackingField(expression As BinaryExpressionSyntax,
                                                                   backingField As IFieldSymbol,
                                                                   semanticModel As SemanticModel) As Boolean

        Return (IsPropertyValueParameter(expression.Right, semanticModel) AndAlso IsBackingField(expression.Left, backingField, semanticModel)) OrElse
            (IsPropertyValueParameter(expression.Left, semanticModel) AndAlso IsBackingField(expression.Right, backingField, semanticModel))
    End Function

End Class
Friend Class ExpandablePropertyInfo
    Public Property BackingFieldName As String
    Public Property NeedsBackingField As Boolean
    Public Property PropertyDeclaration As DeclarationStatementSyntax
    Public Property Type As ITypeSymbol
End Class

Friend Module CodeGeneration

    Friend Function ImplementINotifyPropertyChanged(
            root As CompilationUnitSyntax,
            model As SemanticModel,
            properties As IEnumerable(Of ExpandablePropertyInfo),
            Workspace As Workspace) As CompilationUnitSyntax

        Dim typeDeclaration = properties.First().PropertyDeclaration.FirstAncestorOrSelf(Of TypeBlockSyntax)
        Dim backingFieldLookup = properties.ToDictionary(Function(info) info.PropertyDeclaration, Function(info) info.BackingFieldName)
        Dim allProperties = properties.Select(Function(p) DirectCast(p.PropertyDeclaration, SyntaxNode)).Concat({typeDeclaration})

        root = root.ReplaceNodes(
            allProperties,
            Function(original, updated) ReplaceNode(original, updated, backingFieldLookup, properties, model, Workspace))

        Return root _
            .WithImport("System.Collections.Generic") _
            .WithImport("System.ComponentModel")
    End Function

    Private Function ReplaceNode(
        original As SyntaxNode,
        updated As SyntaxNode,
        backingFieldLookup As Dictionary(Of DeclarationStatementSyntax, String),
        properties As IEnumerable(Of ExpandablePropertyInfo),
        model As SemanticModel,
        workspace As Workspace) As SyntaxNode

        Return If(TypeOf original Is TypeBlockSyntax,
            ExpandType(DirectCast(original, TypeBlockSyntax),
                        DirectCast(updated, TypeBlockSyntax),
                        properties.Where(Function(p) p.NeedsBackingField),
                        model,
                        workspace),
            DirectCast(ExpandProperty(DirectCast(original, DeclarationStatementSyntax), backingFieldLookup(DirectCast(original, DeclarationStatementSyntax))), SyntaxNode))
    End Function

    <Extension>
    Private Function WithImport(root As CompilationUnitSyntax, name As String) As CompilationUnitSyntax
        If Not root.Imports _
            .SelectMany(Function(i) i.ImportsClauses) _
            .Any(Function(i) i.IsKind(SyntaxKind.SimpleImportsClause) AndAlso DirectCast(i, SimpleImportsClauseSyntax).Name.ToString() = name) Then

            Dim clause As ImportsClauseSyntax = SyntaxFactory.SimpleImportsClause(SyntaxFactory.ParseName(name).NormalizeWhitespace(elasticTrivia:=True))
            Dim clauseList = SyntaxFactory.SeparatedList({clause})
            Dim statement = SyntaxFactory.ImportsStatement(clauseList)
            statement = statement.WithAdditionalAnnotations(Formatter.Annotation)

            root = root.AddImports(statement)
        End If

        Return root
    End Function

    Private Function ExpandProperty(propertyDeclaration As DeclarationStatementSyntax, backingFieldName As String) As SyntaxNode
        Dim getter As AccessorBlockSyntax = Nothing
        Dim setter As AccessorBlockSyntax = Nothing
        Dim propertyStatement As PropertyStatementSyntax = Nothing
        Dim propertyBlock As PropertyBlockSyntax = Nothing

        If propertyDeclaration.IsKind(SyntaxKind.PropertyStatement) Then
            propertyStatement = DirectCast(propertyDeclaration, PropertyStatementSyntax)
        ElseIf propertyDeclaration.IsKind(SyntaxKind.PropertyBlock) Then
            propertyBlock = DirectCast(propertyDeclaration, PropertyBlockSyntax)
            propertyStatement = propertyBlock.PropertyStatement

            If Not ExpansionChecker.TryGetAccessors(propertyBlock, getter, setter) Then
                Throw New ArgumentException()
            End If
        Else
            Debug.WriteLine("Unexpected declaration kind.")
        End If

        If getter Is Nothing Then
            getter = SyntaxFactory.AccessorBlock(SyntaxKind.GetAccessorBlock,
                                        SyntaxFactory.AccessorStatement(SyntaxKind.GetAccessorStatement, SyntaxFactory.Token(SyntaxKind.GetKeyword)),
                                        SyntaxFactory.EndBlockStatement(SyntaxKind.EndGetStatement, SyntaxFactory.Token(SyntaxKind.GetKeyword)))
        End If

        Dim returnFieldStatement = SyntaxFactory.ParseExecutableStatement(String.Format("Return {0}", backingFieldName))
        getter = getter.WithStatements(SyntaxFactory.SingletonList(returnFieldStatement))

        If setter Is Nothing Then
            Dim propertyTypeText = DirectCast(propertyStatement.AsClause, SimpleAsClauseSyntax).Type.ToString()
            Dim parameterList = SyntaxFactory.ParseParameterList(String.Format("(value As {0})", propertyTypeText))
            setter = SyntaxFactory.AccessorBlock(SyntaxKind.SetAccessorBlock,
                                            SyntaxFactory.AccessorStatement(SyntaxKind.SetAccessorStatement, SyntaxFactory.Token(SyntaxKind.SetKeyword)).
                                                                     WithParameterList(parameterList),
                                            SyntaxFactory.EndBlockStatement(SyntaxKind.EndSetStatement, SyntaxFactory.Token(SyntaxKind.SetKeyword)))
        End If

        Dim setPropertyStatement = SyntaxFactory.ParseExecutableStatement($"{backingFieldName} = value") 'String.Format("SetProperty({0}, value, ""{1}"")", backingFieldName, propertyStatement.Identifier.ValueText))
        Dim onPropertyInvocation = SyntaxFactory.ParseExecutableStatement($"OnPropertyChanged(NameOf({propertyStatement.Identifier.ValueText}))")

        setter = setter.WithStatements(SyntaxFactory.List({setPropertyStatement, onPropertyInvocation}))

        Dim newPropertyBlock As PropertyBlockSyntax = propertyBlock
        If newPropertyBlock Is Nothing Then
            newPropertyBlock = SyntaxFactory.PropertyBlock(propertyStatement, SyntaxFactory.List(Of AccessorBlockSyntax)())
        End If

        newPropertyBlock = newPropertyBlock.WithAccessors(SyntaxFactory.List({getter, setter}))

        Return newPropertyBlock
    End Function

    Private Function ExpandType(
        original As TypeBlockSyntax,
        updated As TypeBlockSyntax,
        properties As IEnumerable(Of ExpandablePropertyInfo),
        model As SemanticModel,
        workspace As Workspace) As TypeBlockSyntax

        Debug.Assert(original IsNot updated)

        Return updated _
            .WithBackingFields(properties, workspace) _
            .WithBaseType(original, model) _
            .WithPropertyChangedEvent(original, model, workspace) _
            .WithSetPropertyMethod(original, model, workspace) _
            .NormalizeWhitespace(elasticTrivia:=True) _
            .WithAdditionalAnnotations(Formatter.Annotation)
    End Function

    <Extension>
    Private Function WithBackingFields(node As TypeBlockSyntax, properties As IEnumerable(Of ExpandablePropertyInfo), workspace As Workspace) As TypeBlockSyntax

        For Each propertyInfo In properties
            Dim newField = GenerateBackingField(propertyInfo, workspace)
            Dim currentProp = GetProperty(node, GetPropertyName(propertyInfo.PropertyDeclaration))
            node = node.InsertNodesBefore(currentProp, {newField})
        Next

        Return node
    End Function

    Private Function GetPropertyName(node As DeclarationStatementSyntax) As String
        Dim block = TryCast(node, PropertyBlockSyntax)
        If block IsNot Nothing Then
            Return block.PropertyStatement.Identifier.Text
        End If
        Dim prop = TryCast(node, PropertyStatementSyntax)
        If prop IsNot Nothing Then
            Return prop.Identifier.Text
        End If
        Return Nothing
    End Function

    Private Function GetProperty(node As TypeBlockSyntax, name As String) As DeclarationStatementSyntax
        Return node.DescendantNodes().OfType(Of DeclarationStatementSyntax).FirstOrDefault(Function(n) GetPropertyName(n) = name)
    End Function

    Private Function GenerateBackingField(propertyInfo As ExpandablePropertyInfo, workspace As Workspace) As StatementSyntax
        Dim g = SyntaxGenerator.GetGenerator(workspace, LanguageNames.VisualBasic)
        Dim fieldType = g.TypeExpression(propertyInfo.Type)

        Dim fieldDecl = DirectCast(ParseMember(String.Format("Private {0} As _fieldType_", propertyInfo.BackingFieldName)), FieldDeclarationSyntax)
        Return fieldDecl.ReplaceNode(fieldDecl.Declarators(0).AsClause.Type, fieldType).WithAdditionalAnnotations(Formatter.Annotation)
    End Function

    Private Function ParseMember(source As String) As StatementSyntax
        Dim cu = SyntaxFactory.ParseCompilationUnit("Class x" & vbCrLf & source & vbCrLf & "End Class")
        Return DirectCast(cu.Members(0), ClassBlockSyntax).Members(0)
    End Function

    <Extension>
    Private Function AddMembers(node As TypeBlockSyntax, ParamArray members As StatementSyntax()) As TypeBlockSyntax
        Return AddMembers(node, DirectCast(members, IEnumerable(Of StatementSyntax)))
    End Function

    <Extension>
    Private Function AddMembers(node As TypeBlockSyntax, members As IEnumerable(Of StatementSyntax)) As TypeBlockSyntax
        Dim classBlock = TryCast(node, ClassBlockSyntax)
        If classBlock IsNot Nothing Then
            Return classBlock.WithMembers(classBlock.Members.AddRange(members))
        End If

        Dim structBlock = TryCast(node, StructureBlockSyntax)
        If structBlock IsNot Nothing Then
            Return structBlock.WithMembers(structBlock.Members.AddRange(members))
        End If

        Return node
    End Function

    <Extension>
    Private Function WithBaseType(node As TypeBlockSyntax, original As TypeBlockSyntax, model As SemanticModel) As TypeBlockSyntax
        Dim classSymbol = DirectCast(model.GetDeclaredSymbol(original), INamedTypeSymbol)
        Dim interfaceSymbol = model.Compilation.GetTypeByMetadataName(InterfaceName)

        ' Does this class already implement INotifyPropertyChanged? If not, add it to the base list.
        If Not classSymbol.AllInterfaces.Any(Function(i) i.Equals(interfaceSymbol)) Then
            ' Add an annotation to simplify the name
            Dim baseTypeName = SyntaxFactory.ParseTypeName(InterfaceName) _
                .WithAdditionalAnnotations(Simplifier.Annotation)

            ' Add an annotation to format properly.
            Dim implementsStatement = SyntaxFactory.ImplementsStatement(baseTypeName).
                WithAdditionalAnnotations(Formatter.Annotation)

            node = If(node.IsKind(SyntaxKind.ClassBlock),
                DirectCast(DirectCast(node, ClassBlockSyntax).AddImplements(implementsStatement), TypeBlockSyntax),
                DirectCast(node, StructureBlockSyntax).AddImplements(implementsStatement))
        End If

        Return node
    End Function

    Private Const InterfaceName As String = "System.ComponentModel.INotifyPropertyChanged"

    <Extension>
    Private Function WithPropertyChangedEvent(node As TypeBlockSyntax, original As TypeBlockSyntax, model As SemanticModel, workspace As Workspace) As TypeBlockSyntax
        Dim classSymbol = DirectCast(model.GetDeclaredSymbol(original), INamedTypeSymbol)
        Dim interfaceSymbol = model.Compilation.GetTypeByMetadataName(InterfaceName)
        Dim propertyChangedEventSymbol = DirectCast(interfaceSymbol.GetMembers("PropertyChanged").Single(), IEventSymbol)
        Dim propertyChangedEvent = classSymbol.FindImplementationForInterfaceMember(propertyChangedEventSymbol)

        ' Does this class contain an implementation for the PropertyChanged event? If not, add it.
        If propertyChangedEvent Is Nothing Then
            node = AddMembers(node, GeneratePropertyChangedEvent())
        End If

        Return node
    End Function

    Friend Function GeneratePropertyChangedEvent() As StatementSyntax
        Dim decl = ParseMember("Public Event PropertyChanged As System.ComponentModel.PropertyChangedEventHandler Implements System.ComponentModel.INotifyPropertyChanged.PropertyChanged")
        Return decl.WithAdditionalAnnotations(Simplifier.Annotation)
    End Function

    <Extension>
    Private Function WithSetPropertyMethod(node As TypeBlockSyntax, original As TypeBlockSyntax, model As SemanticModel, workspace As Workspace) As TypeBlockSyntax
        Dim classSymbol = DirectCast(model.GetDeclaredSymbol(original), INamedTypeSymbol)
        Dim interfaceSymbol = model.Compilation.GetTypeByMetadataName(InterfaceName)
        Dim propertyChangedEventSymbol = DirectCast(interfaceSymbol.GetMembers("PropertyChanged").Single(), IEventSymbol)
        Dim propertyChangedEvent = classSymbol.FindImplementationForInterfaceMember(propertyChangedEventSymbol)

        Dim setPropertyMethod = classSymbol.FindSetPropertyMethod(model.Compilation)
        If setPropertyMethod Is Nothing Then
            node = AddMembers(node, GenerateSetPropertyMethod())
        End If

        Return node
    End Function

    Friend Function GenerateSetPropertyMethod() As StatementSyntax
        Return ParseMember(<x>
Private Sub OnPropertyChanged(propertyName As String)
    RaiseEvent PropertyChanged(Me, New System.ComponentModel.PropertyChangedEventArgs(propertyName))
End Sub
</x>.Value).WithAdditionalAnnotations(Simplifier.Annotation)

    End Function

    <Extension>
    Private Function FindSetPropertyMethod(classSymbol As INamedTypeSymbol, compilation As Compilation) As IMethodSymbol
        ' Find SetProperty(Of T)(ByRef T, T, string) method.
        Dim setPropertyMethod = classSymbol.
            GetMembers("OnPropertyChanged").OfType(Of IMethodSymbol)().
            FirstOrDefault(Function(m) m.Parameters.Count = 3 AndAlso m.TypeParameters.Count = 1)

        If setPropertyMethod IsNot Nothing Then
            Dim parameters = setPropertyMethod.Parameters

            Dim stringType = compilation.GetSpecialType(SpecialType.System_String)

            If (setPropertyMethod.ReturnsVoid AndAlso
                parameters(0).Type.Equals(stringType)) Then

                Return setPropertyMethod
            End If
        End If

        Return Nothing
    End Function
End Module

