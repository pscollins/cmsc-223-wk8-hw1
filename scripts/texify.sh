# $1 --- the input haskell
# $2 --- the output .tex
# $3 --- the number of the homework assignment

LINOS='\renewcommand{\theFancyVerbLine}{\textcolor{red}{\small\arabic{FancyVerbLine}}}'
LOADFILE='\RecustomVerbatimCommand{\VerbatimInput}{VerbatimInput}{fontsize=\footnotesize,frame=lines,framesep=2em,rulecolor=\color{Gray}'

PREAMB="$LINOS,$LOADFILE"
FMTOPTS="linenos=1,style=perldoc"
TEMPLATE_PATH=$(dirname $0)/template-tex

pygmentize -O $FMTOPTS -f latex -o tmp.tex $1
cp $TEMPLATE_PATH tmp2.tex
sed "/%WORK_HERE/r tmp.tex" tmp2.tex | sed "s/NUMBER/$3/g" > $2
rm tmp.tex tmp2.tex
