using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace DelSole.CrossAnalyzers.CSharp
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class ObservableCollectionAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "CRA001";
        internal static readonly string Title = "List(Of T) is improper for data-binding";
        internal static readonly string MessageFormat = "'{0}' is of type List(Of T). Consider assigning an object of type ObservableCollection(Of T) instead.";
        internal const string Category = "Syntax";

        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSyntaxNodeAction(AnalyzeAssignementStatements, SyntaxKind.SimpleAssignmentExpression);
        }


        public void AnalyzeAssignementStatements(SyntaxNodeAnalysisContext context)
        {
            var node = context.Node as AssignmentExpressionSyntax;
            if (node == null)
            {
                return;
            }

            //Check if the node contains bindable properties
            var leftPart = node.Left.ToFullString();
            if (leftPart.Contains("ItemsSource") == false)
            {
                if (leftPart.Contains("DataContext") == false)
                {
                    return;
                }
            }

            string returnType = "";
            var symbolInfo = context.SemanticModel.GetSymbolInfo(node.Right).Symbol;
            //Check if local variable
            if ((symbolInfo) is ILocalSymbol)
            {
                returnType = ((ILocalSymbol)symbolInfo).Type.ToString();
                //Check if property
            }
            else if ((symbolInfo) is IPropertySymbol)
            {
                returnType = ((IPropertySymbol)symbolInfo).Type.ToString();
                //Check if field
            }
            else if ((symbolInfo) is IFieldSymbol)
            {
                returnType = ((IFieldSymbol)symbolInfo).Type.ToString();
            }
            else if ((symbolInfo) is IMethodSymbol)
            {
                //Check if method
                returnType = ((IMethodSymbol)symbolInfo).ReturnType.ToString();
            }
            else
            {
                return;
            }

            if (returnType.ToLowerInvariant().Contains("system.collections.generic.list"))
            {
                var diagn = Diagnostic.Create(Rule, node.Right.GetLocation(), node.Right.ToString());
                context.ReportDiagnostic(diagn);
            }
        }
    }
}
