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

namespace DelSole.CrossAnalyzers.CSharp
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(MakeRelayCommandRefactoring)), Shared]
    public class MakeRelayCommandRefactoring : CodeRefactoringProvider
    {
        private string Title = "Make RelayCommand<T> class";
        public async sealed override Task
          ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.
              CancellationToken).
                ConfigureAwait(false);
            // Find the node at the selection.
            var node = root.FindNode(context.Span);
            // Only offer a refactoring if the selected node is
            // a class statement node.
            var classDecl = node as ClassDeclarationSyntax;
            if (classDecl == null)
            {
                return;
            }
            var action = CodeAction.Create(title: Title,
              createChangedDocument: c =>
              MakeRelayCommandAsync(context.Document,
              classDecl, c), equivalenceKey: Title);
            // Register this code action.
            context.RegisterRefactoring(action);
        }
        private async Task<Document>
          MakeRelayCommandAsync(Document document,
          ClassDeclarationSyntax classDeclaration, CancellationToken cancellationToken)
        {
            // The class definition represented as source text
            string newImplementation = @"
class RelayCommand<T> : ICommand
{
  readonly Action<T> _execute = null;
  readonly Predicate<T> _canExecute = null;
  public RelayCommand(Action<T> execute)
    : this(execute, null)
  {
  }
  public RelayCommand(Action<T> execute, Predicate<T> canExecute)
  {
    if (execute == null)
      throw new ArgumentNullException(""execute"");
      _execute = execute;
        _canExecute = canExecute;
  }
  [System.Diagnostics.DebuggerStepThrough]
  public bool CanExecute(object parameter)
  {
    return _canExecute == null ? true : _canExecute((T)parameter);
  }
  public event EventHandler CanExecuteChanged;
  public void RaiseCanExecuteChanged()
  {
    var handler = CanExecuteChanged;
    if (handler != null)
    {
      handler(this, EventArgs.Empty);
    }
  }
  public void Execute(object parameter)
  {
    _execute((T)parameter);
  }
}
";
            // 1. ParseSyntaxTree() gets a new SyntaxTree from the source text
            // 2. GetRoot() gets the root node of the tree
            // 3. OfType<ClassDeclarationSyntax>().FirstOrDefault() retrieves the only class
            //      definition in the tree
            // 4. WithAdditionalAnnotations() is invoked for code formatting
            var newClassNode = SyntaxFactory.ParseSyntaxTree(newImplementation).
              GetRoot().DescendantNodes().
              OfType<ClassDeclarationSyntax>().
              FirstOrDefault().
              WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation);
            // Get the root SyntaxNode of the document
            var root = await document.GetSyntaxRootAsync(cancellationToken);
            // Generate a new CompilationUnitSyntax (which represents a code file)
            // replacing the old class with the new one
            CompilationUnitSyntax newRoot = (CompilationUnitSyntax)root.
              ReplaceNode(classDeclaration,
              newClassNode).NormalizeWhitespace();
            if ((newRoot.Usings.Any(u => u.Name.ToFullString() == "System.Windows.Input"))
              == false)
            {
                newRoot = newRoot.AddUsings(SyntaxFactory.UsingDirective(SyntaxFactory.
                  QualifiedName(SyntaxFactory.IdentifierName("System"),
                    SyntaxFactory.IdentifierName("Windows.Input"))));
            }
            // Generate a new document based on the new SyntaxNode
            var newDocument = document.WithSyntaxRoot(newRoot);
            // Return the new document
            return newDocument;
        }

    }
}