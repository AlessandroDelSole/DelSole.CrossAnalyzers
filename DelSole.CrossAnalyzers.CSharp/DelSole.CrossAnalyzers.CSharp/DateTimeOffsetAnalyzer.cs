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
    public class DateTimeOffsetAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "CRA003";
        internal static readonly LocalizableString Title = "Assigning DateTime to objects of type DateTimeOffset might result in runtime errors";
        internal static readonly LocalizableString MessageFormat = "'{0}' should be of type DateTimeOffset";
        internal const string Category = "Platform";

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
            ISymbol leftSymbolInfo = context.SemanticModel.GetSymbolInfo(node.Left).Symbol;

            if (leftSymbolInfo.Kind == SymbolKind.Local) return;

            bool tempSymbol;

            if (leftSymbolInfo.Kind==SymbolKind.Property)
            {
                tempSymbol = ((IPropertySymbol)leftSymbolInfo).Type.ToString().Contains("DateTimeOffset");
                
            }
            else
            {
                tempSymbol = ((IFieldSymbol)leftSymbolInfo).Type.ToString().Contains("DateTimeOffset");
            }

            if (tempSymbol == false) return;

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

            if (returnType.ToLowerInvariant()=="system.datetime")
            {
                var diagn = Diagnostic.Create(Rule, node.Right.GetLocation(), node.Right.ToString());
                context.ReportDiagnostic(diagn);
            }
        }
    }
}