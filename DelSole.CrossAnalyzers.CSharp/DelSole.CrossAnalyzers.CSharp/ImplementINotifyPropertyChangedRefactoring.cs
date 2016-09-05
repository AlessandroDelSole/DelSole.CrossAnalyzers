using System;
using System.Composition;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using Microsoft.CodeAnalysis.Simplification;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Editing;
using System.Diagnostics;

namespace DelSole.CrossAnalyzers.CSharp
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(ImplementINotifyPropertyChangedRefactoring)), Shared]
    public class ImplementINotifyPropertyChangedRefactoring : CodeRefactoringProvider
    {
        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            var document = context.Document;
            var textSpan = context.Span;
            var cancellationToken = context.CancellationToken;

            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false) as CompilationUnitSyntax;
            var model = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            // if length is 0 then no particular range is selected, so pick the first enclosing member
            if (textSpan.Length == 0)
            {
                var decl = root.FindToken(textSpan.Start).Parent.AncestorsAndSelf().OfType<MemberDeclarationSyntax>().FirstOrDefault();
                if (decl != null)
                {
                    textSpan = decl.FullSpan;
                }
            }

            var properties = ExpansionChecker.GetExpandableProperties(textSpan, root, model);

            if (properties.Any())
            {
#pragma warning disable RS0005
                context.RegisterRefactoring(
                   CodeAction.Create("Implement INotifyPropertyChanged interface", (c) =>
                                     ImplementNotifyPropertyChangedAsync(document, root, model, properties, c)));
#pragma warning restore RS0005
            }
        }

        private async Task<Document> ImplementNotifyPropertyChangedAsync(Document document, CompilationUnitSyntax root, SemanticModel model, IEnumerable<ExpandablePropertyInfo> properties, CancellationToken cancellationToken)
        {
            document = document.WithSyntaxRoot(CodeGeneration.ImplementINotifyPropertyChanged(root, model, properties, document.Project.Solution.Workspace));
            document = await Simplifier.ReduceAsync(document, Simplifier.Annotation, cancellationToken: cancellationToken).ConfigureAwait(false);
            document = await Formatter.FormatAsync(document, Formatter.Annotation, cancellationToken: cancellationToken).ConfigureAwait(false);
            return document;
        }
    }

    internal static class CodeGeneration
    {
        internal static CompilationUnitSyntax ImplementINotifyPropertyChanged(CompilationUnitSyntax root, SemanticModel model, IEnumerable<ExpandablePropertyInfo> properties, Workspace workspace)
        {
            var typeDeclaration = properties.First().PropertyDeclaration.FirstAncestorOrSelf<TypeDeclarationSyntax>();
            var backingFieldLookup = Enumerable.ToDictionary(properties, info => info.PropertyDeclaration, info => info.BackingFieldName);

            root = root.ReplaceNodes(properties.Select(p => p.PropertyDeclaration as SyntaxNode).Concat(new[] { typeDeclaration }),
                (original, updated) => original.IsKind(SyntaxKind.PropertyDeclaration)
                    ? ExpandProperty((PropertyDeclarationSyntax)original, backingFieldLookup[(PropertyDeclarationSyntax)original]) as SyntaxNode
                    : ExpandType((TypeDeclarationSyntax)original, (TypeDeclarationSyntax)updated, properties.Where(p => p.NeedsBackingField), model, workspace));

            return root
                .WithUsing("System.Collections.Generic")
                .WithUsing("System.Runtime.CompilerServices")
                .WithUsing("System.ComponentModel");
        }

        private static CompilationUnitSyntax WithUsing(this CompilationUnitSyntax root, string name)
        {
            if (!root.Usings.Any(u => u.Name.ToString() == name))
            {
                root = root.AddUsings(SyntaxFactory.UsingDirective(SyntaxFactory.ParseName(name)).WithAdditionalAnnotations(Formatter.Annotation));
            }

            return root;
        }

        private static TypeDeclarationSyntax ExpandType(TypeDeclarationSyntax original, TypeDeclarationSyntax updated, IEnumerable<ExpandablePropertyInfo> properties, SemanticModel model, Workspace workspace)
        {
            Debug.Assert(original != updated);

            return updated
                .WithBackingFields(properties, workspace)
                .WithBaseType(original, model)
                .WithPropertyChangedEvent(original, model, workspace)
                .WithSetPropertyMethod(original, model, workspace);
        }

        private static TypeDeclarationSyntax WithBackingFields(this TypeDeclarationSyntax node, IEnumerable<ExpandablePropertyInfo> properties, Workspace workspace)
        {
            // generate backing field for auto-props
            foreach (var p in properties)
            {
                var fieldDecl = GenerateBackingField(p, workspace);

                // put field just before property
                var currentProp = node.DescendantNodes().OfType<PropertyDeclarationSyntax>().First(d => d.Identifier.Text == p.PropertyDeclaration.Identifier.Text);
                node = node.InsertNodesBefore(currentProp, new[] { fieldDecl });
            }

            return node;
        }

        private static MemberDeclarationSyntax GenerateBackingField(ExpandablePropertyInfo property, Workspace workspace)
        {
            var g = SyntaxGenerator.GetGenerator(workspace, LanguageNames.CSharp);
            var type = g.TypeExpression(property.Type);

            var fieldDecl = (FieldDeclarationSyntax)ParseMember(string.Format("private _fieldType_ {0};", property.BackingFieldName));
            return fieldDecl.ReplaceNode(fieldDecl.Declaration.Type, type).WithAdditionalAnnotations(Formatter.Annotation);
        }

        private static MemberDeclarationSyntax ParseMember(string member)
        {
            var decl = ((ClassDeclarationSyntax)SyntaxFactory.ParseCompilationUnit("class x {\r\n" + member + "\r\n}").Members[0]).Members[0];
            return decl.WithAdditionalAnnotations(Formatter.Annotation);
        }

        private static TypeDeclarationSyntax AddMembers(this TypeDeclarationSyntax node, params MemberDeclarationSyntax[] members)
        {
            return AddMembers(node, (IEnumerable<MemberDeclarationSyntax>)members);
        }

        private static TypeDeclarationSyntax AddMembers(this TypeDeclarationSyntax node, IEnumerable<MemberDeclarationSyntax> members)
        {
            var classDecl = node as ClassDeclarationSyntax;
            if (classDecl != null)
            {
                return classDecl.WithMembers(classDecl.Members.AddRange(members));
            }

            var structDecl = node as StructDeclarationSyntax;
            if (structDecl != null)
            {
                return structDecl.WithMembers(structDecl.Members.AddRange(members));
            }

            throw new InvalidOperationException();
        }

        private static PropertyDeclarationSyntax ExpandProperty(PropertyDeclarationSyntax property, string backingFieldName)
        {
            AccessorDeclarationSyntax getter, setter;
            if (!ExpansionChecker.TryGetAccessors(property, out getter, out setter))
            {
                throw new ArgumentException();
            }

            if (getter.Body == null)
            {
                var returnFieldStatement = SyntaxFactory.ParseStatement(string.Format("return {0};", backingFieldName));
                getter = getter
                    .WithBody(SyntaxFactory.Block(SyntaxFactory.SingletonList(returnFieldStatement)));
            }

            getter = getter
                .WithSemicolonToken(default(SyntaxToken));

            var setPropertyStatement = SyntaxFactory.ParseStatement($"{backingFieldName} = value;");    //string.Format("SetProperty(ref {0}, value, \"{1}\");", backingFieldName, property.Identifier.ValueText));
            var changeInvocation = SyntaxFactory.ParseStatement("OnPropertyChanged();");

            setter = setter
                .WithBody(SyntaxFactory.Block(new List<StatementSyntax>() { setPropertyStatement, changeInvocation }.ToArray()))
                .WithSemicolonToken(default(SyntaxToken));

            var newProperty = property
                .WithAccessorList(SyntaxFactory.AccessorList(SyntaxFactory.List(new[] { getter, setter })))
                .WithAdditionalAnnotations(Formatter.Annotation);

            return newProperty;
        }

        private const string InterfaceName = "System.ComponentModel.INotifyPropertyChanged";

        private static TypeDeclarationSyntax WithBaseType(this TypeDeclarationSyntax node, TypeDeclarationSyntax original, SemanticModel semanticModel)
        {
            var classSymbol = semanticModel.GetDeclaredSymbol(original);
            var interfaceSymbol = semanticModel.Compilation.GetTypeByMetadataName(InterfaceName);

            // Does this class already implement INotifyPropertyChanged? If not, add it to the base list.
            if (!classSymbol.AllInterfaces.Contains(interfaceSymbol))
            {
                var baseTypeName = SyntaxFactory.ParseTypeName(InterfaceName)
                    .WithAdditionalAnnotations(Simplifier.Annotation);

                node = node.IsKind(SyntaxKind.ClassDeclaration)
                   ? ((ClassDeclarationSyntax)node).AddBaseListTypes(SyntaxFactory.SimpleBaseType(baseTypeName)) as TypeDeclarationSyntax
                   : ((StructDeclarationSyntax)node).AddBaseListTypes(SyntaxFactory.SimpleBaseType(baseTypeName));

                // Add a formatting annotation to the base list to ensure that it gets formatted properly.
                node = node.ReplaceNode(
                    node.BaseList,
                    node.BaseList.WithAdditionalAnnotations(Formatter.Annotation));
            }

            return node;
        }

        private static TypeDeclarationSyntax WithPropertyChangedEvent(this TypeDeclarationSyntax node, TypeDeclarationSyntax original, SemanticModel semanticModel, Workspace workspace)
        {
            var classSymbol = semanticModel.GetDeclaredSymbol(original);
            var interfaceSymbol = semanticModel.Compilation.GetTypeByMetadataName(InterfaceName);
            var propertyChangedEventSymbol = (IEventSymbol)interfaceSymbol.GetMembers("PropertyChanged").Single();
            var propertyChangedEvent = classSymbol.FindImplementationForInterfaceMember(propertyChangedEventSymbol);

            // Does this class contain an implementation for the PropertyChanged event? If not, add it.
            if (propertyChangedEvent == null)
            {
                var propertyChangedEventDecl = GeneratePropertyChangedEvent();
                node = node.AddMembers(propertyChangedEventDecl);
            }

            return node;
        }

        internal static MemberDeclarationSyntax GeneratePropertyChangedEvent()
        {
            var decl = (EventFieldDeclarationSyntax)ParseMember("public event System.ComponentModel.PropertyChangedEventHandler PropertyChanged;");
            return decl.ReplaceNode(decl.Declaration.Type, decl.Declaration.Type.WithAdditionalAnnotations(Simplifier.Annotation));
        }

        private static IMethodSymbol FindSetPropertyMethod(this INamedTypeSymbol classSymbol, Compilation compilation)
        {
            // Find SetProperty<T>(ref T, T, string) method.
            var setPropertyMethod = classSymbol
                .GetMembers("OnPropertyChanged")
                .OfType<IMethodSymbol>()
                .FirstOrDefault(m => m.Parameters.Length == 3 && m.TypeParameters.Length == 1);

            if (setPropertyMethod != null)
            {
                var parameters = setPropertyMethod.Parameters;
                //var typeParameter = setPropertyMethod.TypeParameters[0];
                var stringType = compilation.GetSpecialType(SpecialType.System_String);

                if (setPropertyMethod.ReturnsVoid &&
                    parameters[0].Type.Equals(stringType))
                {
                    return setPropertyMethod;
                }
            }

            return null;
        }

        private static TypeDeclarationSyntax WithSetPropertyMethod(this TypeDeclarationSyntax node, TypeDeclarationSyntax original, SemanticModel semanticModel, Workspace workspace)
        {
            var classSymbol = semanticModel.GetDeclaredSymbol(original);
            var interfaceSymbol = semanticModel.Compilation.GetTypeByMetadataName(InterfaceName);
            var propertyChangedEventSymbol = (IEventSymbol)interfaceSymbol.GetMembers("PropertyChanged").Single();
            var propertyChangedEvent = classSymbol.FindImplementationForInterfaceMember(propertyChangedEventSymbol);

            var setPropertyMethod = classSymbol.FindSetPropertyMethod(semanticModel.Compilation);
            if (setPropertyMethod == null)
            {
                var setPropertyDecl = GenerateSetPropertyMethod();
                node = AddMembers(node, setPropertyDecl);
            }

            return node;
        }

        internal static MethodDeclarationSyntax GenerateSetPropertyMethod()
        {
            return (MethodDeclarationSyntax)ParseMember(@"
protected void OnPropertyChanged([CallerMemberName] string propertyName = null)
{
    PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
}").WithAdditionalAnnotations(Simplifier.Annotation);
        }
    }


    internal class ExpandablePropertyInfo
    {
        public string BackingFieldName { get; internal set; }
        public bool NeedsBackingField { get; internal set; }
        public PropertyDeclarationSyntax PropertyDeclaration { get; internal set; }
        public ITypeSymbol Type { get; internal set; }
    }

    internal static class ExpansionChecker
    {
        internal static IEnumerable<ExpandablePropertyInfo> GetExpandableProperties(TextSpan span, SyntaxNode root, SemanticModel model)
        {
            var propertiesInTypes = root.DescendantNodes(span)
                       .OfType<PropertyDeclarationSyntax>()
                       .Select(p => GetExpandablePropertyInfo(p, model))
                       .Where(p => p != null)
                       .GroupBy(p => p.PropertyDeclaration.FirstAncestorOrSelf<TypeDeclarationSyntax>());

            return propertiesInTypes.Any()
                ? propertiesInTypes.First()
                : Enumerable.Empty<ExpandablePropertyInfo>();
        }

        /// <summary>
        /// Returns true if the specified <see cref="PropertyDeclarationSyntax"/> can be expanded to include
        /// support for <see cref="INotifyPropertyChanged"/>.
        /// </summary>
        internal static ExpandablePropertyInfo GetExpandablePropertyInfo(PropertyDeclarationSyntax property, SemanticModel model)
        {
            // Don't expand properties with parse errors.
            if (property.ContainsDiagnostics)
            {
                return null;
            }

            if (property.Modifiers.Any(SyntaxKind.StaticKeyword) || property.Modifiers.Any(SyntaxKind.AbstractKeyword))
            {
                return null;
            }

            AccessorDeclarationSyntax getter, setter;
            if (!TryGetAccessors(property, out getter, out setter))
            {
                return null;
            }

            if (getter.Body == null && setter.Body == null)
            {
                return new ExpandablePropertyInfo
                {
                    PropertyDeclaration = property,
                    BackingFieldName = GenerateFieldName(property, model),
                    NeedsBackingField = true,
                    Type = model.GetDeclaredSymbol(property).Type
                };
            }

            IFieldSymbol backingField;
            return (TryGetBackingFieldFromExpandableGetter(getter, model, out backingField)
                && IsExpandableSetter(setter, model, backingField))
                ? new ExpandablePropertyInfo { PropertyDeclaration = property, BackingFieldName = backingField.Name }
                : null;
        }

        /// <summary>
        /// Retrieves the get and set accessor declarations of the specified property.
        /// Returns true if both get and set accessors exist; otherwise false.
        /// </summary>
        internal static bool TryGetAccessors(
            PropertyDeclarationSyntax property,
            out AccessorDeclarationSyntax getter,
            out AccessorDeclarationSyntax setter)
        {
            var accessors = property.AccessorList.Accessors;
            getter = accessors.FirstOrDefault(ad => ad.Kind() == SyntaxKind.GetAccessorDeclaration);
            setter = accessors.FirstOrDefault(ad => ad.Kind() == SyntaxKind.SetAccessorDeclaration);

            return accessors.Count == 2 && getter != null && setter != null;
        }

        private static string GenerateFieldName(PropertyDeclarationSyntax property, SemanticModel semanticModel)
        {
            var baseName = property.Identifier.ValueText;
            baseName = char.ToLower(baseName[0]).ToString() + baseName.Substring(1);

            var propertySymbol = semanticModel.GetDeclaredSymbol(property);
            if (propertySymbol == null ||
                propertySymbol.ContainingType == null)
            {
                return baseName;
            }

            var index = 0;
            var name = baseName;
            while (propertySymbol.ContainingType.MemberNames.Contains(name))
            {
                name = baseName + (++index).ToString();
            }

            return name;
        }

        private static IFieldSymbol GetBackingFieldFromGetter(
            AccessorDeclarationSyntax getter,
            SemanticModel semanticModel)
        {
            // The getter should have a body containing a single return of a backing field.

            if (getter.Body == null)
            {
                return null;
            }

            var statements = getter.Body.Statements;
            if (statements.Count != 1)
            {
                return null;
            }

            var returnStatement = statements.Single() as ReturnStatementSyntax;
            if (returnStatement == null || returnStatement.Expression == null)
            {
                return null;
            }

            return semanticModel.GetSymbolInfo(returnStatement.Expression).Symbol as IFieldSymbol;
        }

        private static bool TryGetBackingFieldFromExpandableGetter(
            AccessorDeclarationSyntax getter,
            SemanticModel semanticModel,
            out IFieldSymbol backingField)
        {
            backingField = GetBackingFieldFromGetter(getter, semanticModel);
            return backingField != null;
        }

        private static bool IsBackingField(ExpressionSyntax expression, IFieldSymbol backingField, SemanticModel semanticModel)
        {
            return semanticModel
                .GetSymbolInfo(expression).Symbol
                .Equals(backingField);
        }

        private static bool IsPropertyValueParameter(ExpressionSyntax expression, SemanticModel semanticModel)
        {
            var parameterSymbol = semanticModel.GetSymbolInfo(expression).Symbol as IParameterSymbol;

            return parameterSymbol != null
                && parameterSymbol.IsImplicitlyDeclared
                && parameterSymbol.Name == "value";
        }

        private static bool IsAssignmentOfPropertyValueParameterToBackingField(
            ExpressionSyntax expression,
            IFieldSymbol backingField,
            SemanticModel semanticModel)
        {
            if (expression.Kind() != SyntaxKind.SimpleAssignmentExpression)
            {
                return false;
            }

            var assignment = (AssignmentExpressionSyntax)expression;

            return IsBackingField(assignment.Left, backingField, semanticModel)
                && IsPropertyValueParameter(assignment.Right, semanticModel);
        }

        private static bool ComparesPropertyValueParameterAndBackingField(
            BinaryExpressionSyntax expression,
            IFieldSymbol backingField,
            SemanticModel semanticModel)
        {
            return (IsPropertyValueParameter(expression.Right, semanticModel) && IsBackingField(expression.Left, backingField, semanticModel))
                || (IsPropertyValueParameter(expression.Left, semanticModel) && IsBackingField(expression.Right, backingField, semanticModel));
        }

        private static bool IsExpandableSetter(AccessorDeclarationSyntax setter, SemanticModel semanticModel, IFieldSymbol backingField)
        {
            // The setter should have a body containing one of the following heuristic patterns or
            // no body at all.
            //
            // Patterns:
            //    field = value;
            //    if (field != value) field = value;
            //    if (field == value) return; field = value;

            if (setter.Body == null)
            {
                return false;
            }

            return IsExpandableSetterPattern1(setter, backingField, semanticModel)
                || IsExpandableSetterPattern2(setter, backingField, semanticModel)
                || IsExpandableSetterPattern3(setter, backingField, semanticModel);
        }

        private static bool IsExpandableSetterPattern1(
            AccessorDeclarationSyntax setter,
            IFieldSymbol backingField,
            SemanticModel semanticModel)
        {
            // Pattern: field = value
            Debug.Assert(setter.Body != null);

            var statements = setter.Body.Statements;
            if (statements.Count != 1)
            {
                return false;
            }

            var expressionStatement = statements[0] as ExpressionStatementSyntax;
            return expressionStatement != null
                && IsAssignmentOfPropertyValueParameterToBackingField(expressionStatement.Expression, backingField, semanticModel);
        }

        private static bool IsExpandableSetterPattern2(
            AccessorDeclarationSyntax setter,
            IFieldSymbol backingField,
            SemanticModel semanticModel)
        {
            // Pattern: if (field != value) field = value;
            Debug.Assert(setter.Body != null);

            var statements = setter.Body.Statements;
            if (statements.Count != 1)
            {
                return false;
            }

            var ifStatement = statements[0] as IfStatementSyntax;
            if (ifStatement == null)
            {
                return false;
            }

            var statement = ifStatement.Statement;
            if (statement is BlockSyntax)
            {
                var blockStatements = ((BlockSyntax)statement).Statements;
                if (blockStatements.Count != 1)
                {
                    return false;
                }

                statement = blockStatements[0];
            }

            var expressionStatement = statement as ExpressionStatementSyntax;
            if (expressionStatement == null)
            {
                return false;
            }

            if (!IsAssignmentOfPropertyValueParameterToBackingField(expressionStatement.Expression, backingField, semanticModel))
            {
                return false;
            }

            var condition = ifStatement.Condition as BinaryExpressionSyntax;
            if (condition == null ||
                condition.Kind() != SyntaxKind.NotEqualsExpression)
            {
                return false;
            }

            return ComparesPropertyValueParameterAndBackingField(condition, backingField, semanticModel);
        }

        private static bool IsExpandableSetterPattern3(
            AccessorDeclarationSyntax setter,
            IFieldSymbol backingField,
            SemanticModel semanticModel)
        {
            // Pattern: if (field == value) return; field = value;

            Debug.Assert(setter.Body != null);

            var statements = setter.Body.Statements;
            if (statements.Count != 2)
            {
                return false;
            }

            var ifStatement = statements[0] as IfStatementSyntax;
            if (ifStatement == null)
            {
                return false;
            }

            var statement = ifStatement.Statement;
            if (statement is BlockSyntax)
            {
                var blockStatements = ((BlockSyntax)statement).Statements;
                if (blockStatements.Count != 1)
                {
                    return false;
                }

                statement = blockStatements[0];
            }

            var returnStatement = statement as ReturnStatementSyntax;
            if (returnStatement == null ||
                returnStatement.Expression != null)
            {
                return false;
            }

            var expressionStatement = statements[1] as ExpressionStatementSyntax;
            if (expressionStatement == null)
            {
                return false;
            }

            if (!IsAssignmentOfPropertyValueParameterToBackingField(expressionStatement.Expression, backingField, semanticModel))
            {
                return false;
            }

            var condition = ifStatement.Condition as BinaryExpressionSyntax;
            if (condition == null ||
                condition.Kind() != SyntaxKind.EqualsExpression)
            {
                return false;
            }

            return ComparesPropertyValueParameterAndBackingField(condition, backingField, semanticModel);
        }
    }

}