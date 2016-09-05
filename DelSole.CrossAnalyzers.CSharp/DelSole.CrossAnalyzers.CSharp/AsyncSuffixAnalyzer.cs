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
    public class AsyncSuffixAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "CRA002";
        static internal readonly LocalizableString Title = "Names of asynchronous methods whould end with Async";
        static internal readonly LocalizableString MessageFormat = "Name of asynchronous method '{0}' does not end with Async";
        internal const string Category = "Naming";
        static internal DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, true, helpLinkUri: "https://github.com/AlessandroDelSole/DelSole.CrossAnalyzers/wiki");

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterSymbolAction(AnalyzeSymbol, SymbolKind.Method);
        }


        private void AnalyzeSymbol(SymbolAnalysisContext context)
        {
            // TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find

            var methodSymbol = (IMethodSymbol)context.Symbol;

            if (methodSymbol.IsAsync == false)
            {
                return;
            }

            if (!methodSymbol.Name.ToLowerInvariant().EndsWith("async"))
            {
                var diag = Diagnostic.Create(Rule, methodSymbol.Locations[0], methodSymbol.Name);

                context.ReportDiagnostic(diag);
            }
        }
    }
}