<div class="card mt-0 mt-lg-3">
  $if(revision)$
  <h2 class="revision card-header alert alert-warning d-flex justify-content-between align-items-center">
    <span class="font-weight-normal">Revision</span>
    <code class="badge badge-warning ml-1 mr-auto">$revision$</code>
    <a class="btn btn-primary ml-3" href="$base$$pageUrl$">Current version</a>
  </h2>
  $endif$
  <div id="content" class="card-body clearfix my-0">
    <h1 class="pageTitle my-3">$pagetitle$</h1>
    $if(messages)$
$messages()$
    $endif$
     <div style="display: none">
            \\(\\newcommand{\\R}{\\mathbb{R}}
            \\newcommand{\\Z}{\\mathbb{Z}}
            \\newcommand{\\Q}{\\mathbb{Q}}
            \\newcommand{\\C}{\\mathbb{C}}
            \\newcommand{\\N}{\\mathbb{N}}
            \\renewcommand{\\S}{\\cl{S}}
            \\renewcommand{\\P}{\\mathbb{P}}
            
\\newcommand{\\bA}{\\mathbb{A}}
\\newcommand{\\bB}{\\mathbb{B}}
\\newcommand{\\bC}{\\mathbb{C}}
\\newcommand{\\bD}{\\mathbb{D}}
\\newcommand{\\bE}{\\mathbb{E}}
\\newcommand{\\bF}{\\mathbb{F}}
\\newcommand{\\bG}{\\mathbb{G}}
\\newcommand{\\bH}{\\mathbb{H}}
\\newcommand{\\bI}{\\mathbb{I}}
\\newcommand{\\bJ}{\\mathbb{J}}
\\newcommand{\\bK}{\\mathbb{K}}
\\newcommand{\\bL}{\\mathbb{L}}
\\newcommand{\\bM}{\\mathbb{M}}
\\newcommand{\\bN}{\\mathbb{N}}
\\newcommand{\\bO}{\\mathbb{O}}
\\newcommand{\\bP}{\\mathbb{P}}
\\newcommand{\\bQ}{\\mathbb{Q}}
\\newcommand{\\bR}{\\mathbb{R}}
\\newcommand{\\bS}{\\mathbb{S}}
\\newcommand{\\bT}{\\mathbb{T}}
\\newcommand{\\bU}{\\mathbb{U}}
\\newcommand{\\bV}{\\mathbb{V}}
\\newcommand{\\bW}{\\mathbb{W}}
\\newcommand{\\bX}{\\mathbb{X}}
\\newcommand{\\bY}{\\mathbb{Y}}
\\newcommand{\\bZ}{\\mathbb{Z}}
\\newcommand{\\cA}{\\mathcal{A}}
\\newcommand{\\cB}{\\mathcal{B}}
\\newcommand{\\cC}{\\mathcal{C}}
\\newcommand{\\cD}{\\mathcal{D}}
\\newcommand{\\cE}{\\mathcal{E}}
\\newcommand{\\cF}{\\mathcal{F}}
\\newcommand{\\cG}{\\mathcal{G}}
\\newcommand{\\cH}{\\mathcal{H}}
\\newcommand{\\cI}{\\mathcal{I}}
\\newcommand{\\cJ}{\\mathcal{J}}
\\newcommand{\\cK}{\\mathcal{K}}
\\newcommand{\\cL}{\\mathcal{L}}
\\newcommand{\\cM}{\\mathcal{M}}
\\newcommand{\\cN}{\\mathcal{N}}
\\newcommand{\\cO}{\\mathcal{O}}
\\newcommand{\\cP}{\\mathcal{P}}
\\newcommand{\\cQ}{\\mathcal{Q}}
\\newcommand{\\cR}{\\mathcal{R}}
\\newcommand{\\cS}{\\mathcal{S}}
\\newcommand{\\cT}{\\mathcal{T}}
\\newcommand{\\cU}{\\mathcal{U}}
\\newcommand{\\cV}{\\mathcal{V}}
\\newcommand{\\cW}{\\mathcal{W}}
\\newcommand{\\cX}{\\mathcal{X}}
\\newcommand{\\cY}{\\mathcal{Y}}
\\newcommand{\\cZ}{\\mathcal{Z}}
\\newcommand{\\sA}{\\mathscr{A}}
\\newcommand{\\sB}{\\mathscr{B}}
\\newcommand{\\sC}{\\mathscr{C}}
\\newcommand{\\sD}{\\mathscr{D}}
\\newcommand{\\sE}{\\mathscr{E}}
\\newcommand{\\sF}{\\mathscr{F}}
\\newcommand{\\sG}{\\mathscr{G}}
\\newcommand{\\sH}{\\mathscr{H}}
\\newcommand{\\sI}{\\mathscr{I}}
\\newcommand{\\sJ}{\\mathscr{J}}
\\newcommand{\\sK}{\\mathscr{K}}
\\newcommand{\\sL}{\\mathscr{L}}
\\newcommand{\\sM}{\\mathscr{M}}
\\newcommand{\\sN}{\\mathscr{N}}
\\newcommand{\\sO}{\\mathscr{O}}
\\newcommand{\\sP}{\\mathscr{P}}
\\newcommand{\\sQ}{\\mathscr{Q}}
\\newcommand{\\sR}{\\mathscr{R}}
\\newcommand{\\sS}{\\mathscr{S}}
\\newcommand{\\sT}{\\mathscr{T}}
\\newcommand{\\sU}{\\mathscr{U}}
\\newcommand{\\sV}{\\mathscr{V}}
\\newcommand{\\sW}{\\mathscr{W}}
\\newcommand{\\sX}{\\mathscr{X}}
\\newcommand{\\sY}{\\mathscr{Y}}
\\newcommand{\\sZ}{\\mathscr{Z}}

            
            \\newcommand{\\inv}{^{-1}}
            \\newcommand{\\op}{^{op}}
            \\newcommand{\\into}{\\hookrightarrow}
            \\DeclareMathOperator{\\map}{map}
            \\DeclareMathOperator{\\Map}{Map}
            \\DeclareMathOperator{\\Fun}{Fun}
            \\DeclareMathOperator{\\Hom}{Hom}
            \\DeclareMathOperator{\\CAlg}{CAlg}
            
            \\DeclareMathOperator{\\Alg}{Alg}
            \\DeclareMathOperator{\\Id}{Id}
            \\DeclareMathOperator{\\Spec}{Spec}
            \\DeclareMathOperator{\\cofib}{cofib}
            \\DeclareMathOperator{\\fib}{fib}
            \\DeclareMathOperator{\\colim}{colim}
            \\DeclareMathOperator{\\hocolim}{hocolim}
            \\newcommand{\\braket}[1]{\\langle #1 \\rangle}
            \\newcommand{\\ang}{\\braket}
            \\newcommand{\\norm}[1]{\\left\\lvert #1 \\right\\rvert}
            \\DeclareMathOperator{\\gr}{gr}
            \\DeclareMathOperator{\\Cat}{\\cl{C}at}
            \\DeclareMathOperator{\\Sp}{Sp}
            \\DeclareMathOperator{\\Rel}{Rel}
            \\DeclareMathOperator{\\Top}{Top}
            \\DeclareMathOperator{\\Set}{Set}
            \\DeclareMathOperator{\\sSet}{sSet}
            \\DeclareMathOperator{\\Fin}{Fin}
            \\DeclareMathOperator{\\Mod}{Mod}
            \\DeclareMathOperator{\\stab}{stab}
            \\DeclareMathOperator{\\Stone}{Stone}
            \\DeclareMathOperator{\\Meas}{Meas}
            \\DeclareMathOperator{\\CHaus}{CHaus}
            \\DeclareMathOperator{\\ev}{ev}
            \\DeclareMathOperator{\\Ult}{Ult}
            \\DeclareMathOperator{\\LUlt}{LUlt}
            \\DeclareMathOperator{\\Pyk}{Pyk}
            \\DeclareMathOperator{\\Triv}{Triv}

            \\newcommand{\\ol}{\\overline}
            \\newcommand{\\osum}{\\bigoplus}
            \\newcommand{\\tensor}{\\otimes}
            \\newcommand{\\lift}{\\boxslash}
            \\newcommand{\\popr}{\\boxempty}
            \\newcommand{\\from}{\\leftarrow}
            
            \\newcommand{\\labelto}[1]{\\overset{#1}{\\to}}
            
            \\let\\oldphi\\phi
            \\let\\oldepsilon\\epsilon
            \\renewcommand{\\phi}{\\varphi}
            \\renewcommand{\\epsilon}{\\varepsilon}
            
            \\newcommand{\\cl}{\\mathcal}
            \\newcommand{\\catname}[1]{\\mathbf{#1}}
            \\newcommand{\\mscr}[1]{\\mathscr{#1}}
            \\newcommand{\\frk}[1]{\\mathfrak{#1}}
            
            
            \\newcommand{\\rtri}{\\triangleright}
            \\newcommand{\\ltri}{\\triangleleft}
           
            
            \\newcommand{\\myNorm}[2]{
            \\left\\|#1\\right\\|_{#2}
            }
            
            
            
            \\newcommand{\\vect}[1]{\\overset{\\rightharpoonup}{#1}}
            
            
            
            \\newcommand{\\diag}{{\\rm diag}}
            \\newcommand{\\myspan}{{\\rm span}}
            \\newcommand{\\Mat}{{\\rm Mat}}
            \\newcommand{\\Ker}{{\\rm Ker}}
            \\newcommand{\\sign}{{\\rm sgn}}
            \\newcommand{\\tr}{{\\rm tr}\\,}
            
            
            \\newcommand{\\mydn}[3]{\\frac{d^{#3}#1}{d#2^{#3}}}
            \\newcommand{\\myd}[2]{\\frac{d #1}{d#2}}
            \\newcommand{\\myDn}[3]{\\frac{\\partial^{#3}#1}{\\partial #2^{#3}}}
            \\newcommand{\\myD}[2]{\\frac{\\partial #1}{\\partial #2}}
            
        
            \\newcommand{\\isdef}{:=}
            
           
            \\newcommand{\\BBC}{{\\mathbb C}}
            \\newcommand{\\BBR}{{\\mathbb R}}
            \\newcommand{\\BBU}{{\\mathbb U}}
            \\newcommand{\\BBX}{{\\mathbb X}}\\) </div>
$content$

</div>
</div>