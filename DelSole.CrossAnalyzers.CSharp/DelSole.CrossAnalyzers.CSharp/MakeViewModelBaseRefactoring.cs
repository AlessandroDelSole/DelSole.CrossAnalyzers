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
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(MakeViewModelBaseRefactoring)), Shared]
    internal class MakeViewModelBaseRefactoring : CodeRefactoringProvider
    {
        private string Title = "Make ViewModelBase class";
        public async sealed override Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            // Get the root node of the syntax tree
            var root = await context.Document.
              GetSyntaxRootAsync(context.CancellationToken).
              ConfigureAwait(false);
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
              MakeViewModelBaseAsync(context.Document,
                classDecl, c), equivalenceKey: Title);
            // Register this code action.
            context.RegisterRefactoring(action);
        }

        private async Task<Document> MakeViewModelBaseAsync(Document document,
  ClassDeclarationSyntax classDeclaration, CancellationToken cancellationToken)
        {
            // The class definition represented as source text
            string newImplementation = @"abstract class ViewModelBase : INotifyPropertyChanged
{
public event System.ComponentModel.PropertyChangedEventHandler PropertyChanged;
// Raise a property change notification
protected virtual void OnPropertyChanged(string propertyName)
{
  PropertyChanged?.Invoke(this, new System.ComponentModel.
    PropertyChangedEventArgs(propertyName));
}
}
";
            // 1. ParseSyntaxTree() gets a new SyntaxTree from the source text
            // 2. GetRoot() gets the root node of the tree
            // 3. OfType<ClassDeclarationSyntax>().FirstOrDefault()
            //    retrieves the only class definition in the tree
            // 4. WithAdditionalAnnotations() is invoked for code formatting
            var newClassNode = SyntaxFactory.ParseSyntaxTree(newImplementation).
              GetRoot().DescendantNodes().
              OfType<ClassDeclarationSyntax>().
              FirstOrDefault().
              WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation);
            // Get the root SyntaxNode of the document
            var root = await document.GetSyntaxRootAsync();
            // Generate a new CompilationUnitSyntax (which represents a code file)
            // replacing the old class with the new one
            CompilationUnitSyntax newRoot = (CompilationUnitSyntax)root.
              ReplaceNode(classDeclaration, newClassNode).NormalizeWhitespace();
            // Detect if a using System.ComponentModel directive already exists.
            if ((newRoot.Usings.Any(u => u.Name.ToFullString() ==
              "System.ComponentModel")) == false)
            {
                // If not, add one
                newRoot = newRoot.AddUsings(SyntaxFactory.UsingDirective(SyntaxFactory.
                  QualifiedName(SyntaxFactory.IdentifierName("System"),
                                SyntaxFactory.IdentifierName("ComponentModel"))));
            }
            // Generate a new document based on the new SyntaxNode
            var newDocument = document.WithSyntaxRoot(newRoot);
            // Return the new document
            return newDocument;
        }

    }
}