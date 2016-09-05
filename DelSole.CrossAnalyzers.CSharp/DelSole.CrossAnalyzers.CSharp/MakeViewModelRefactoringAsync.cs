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
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Simplification;

namespace DelSole.CrossAnalyzers.CSharp
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(MakeViewModelRefactoringAsync)), Shared]
    public class MakeViewModelRefactoringAsync : CodeRefactoringProvider
    {
        private string Title = "Make ViewModel class";

        public async sealed override Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            // Get the root node of the syntax tree
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            // Find the node at the selection.
            var node = root.FindNode(context.Span);
            // Is this a class statement node?
            var classDecl = node as ClassDeclarationSyntax;
            if (classDecl == null)
            {
                return;
            }
            // If so, create an action to offer a refactoring
            var action = CodeAction.Create(title: Title,
              createChangedDocument: c =>
              MakeViewModelAsync(context.Document,
                classDecl, c), equivalenceKey: Title);
            // Register this code action.
            context.RegisterRefactoring(action);
        }

        private async Task<Document> MakeViewModelAsync(Document document,
  ClassDeclarationSyntax classDeclaration, CancellationToken cancellationToken)
        {
            // Get the name of the model class
            var modelClassName = classDeclaration.Identifier.Text;
            // The name of the ViewModel class
            var viewModelClassName = $"{modelClassName}ViewModel";
            // Only for demo purposes, pluralizing an object is done by
            // simply adding the "s" letter. Consider proper algorithms
            string newImplementation = $@"class {viewModelClassName} : INotifyPropertyChanged
{{
public event PropertyChangedEventHandler PropertyChanged;
// Raise a property change notification
protected virtual void OnPropertyChanged(string propname)
{{
  PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propname));
}}
private ObservableCollection<{modelClassName}> _{modelClassName}s;
public ObservableCollection<{modelClassName}> {modelClassName}s
{{
  get {{ return _{modelClassName}s; }}
  set
  {{
    _{modelClassName}s = value;
    OnPropertyChanged(nameof({modelClassName}s));
  }}
}}
public {viewModelClassName}() {{
// Implement your logic to load a collection of items
}}
}}
";
            var newClassNode = SyntaxFactory.ParseSyntaxTree(newImplementation).
              GetRoot().DescendantNodes().
              OfType<ClassDeclarationSyntax>().
              FirstOrDefault().
              WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation);
            // Retrieve the parent namespace declaration
            var parentNamespace = (NamespaceDeclarationSyntax)classDeclaration.Parent;
            //Add the new class to the namespace
            var newParentNamespace =
              parentNamespace.AddMembers(newClassNode).NormalizeWhitespace();
            var root = await document.GetSyntaxRootAsync(cancellationToken);
            CompilationUnitSyntax newRoot = (CompilationUnitSyntax)root;
            newRoot = newRoot.
                      ReplaceNode(parentNamespace, newParentNamespace).NormalizeWhitespace();
            newRoot = newRoot.AddUsings(SyntaxFactory.UsingDirective(SyntaxFactory.
              QualifiedName(SyntaxFactory.IdentifierName("System"),
                SyntaxFactory.IdentifierName("Collections.ObjectModel"))),
                SyntaxFactory.UsingDirective(SyntaxFactory.
              QualifiedName(SyntaxFactory.IdentifierName("System"),
                SyntaxFactory.IdentifierName("ComponentModel"))));
            // Generate a new document based on the new SyntaxNode
            var newDocument = document.WithSyntaxRoot(newRoot);
            // Return the new document
            return newDocument;
        }

    }
}