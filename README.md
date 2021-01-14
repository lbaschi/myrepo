---
output:
  pdf_document: default
  html_document: default
---
%%==================================%%
%%% Specification of document type %%%
%%==================================%%
\documentclass[a4paper, twoside]{article}
%\documentclass[draft]{article}
\usepackage{dirtytalk}
\usepackage{graphicx}
\usepackage{kantlipsum}
\usepackage{microtype}
\usepackage{physics}
\usepackage{bbding} % \Checkmark and \CheckmarkBold
\usepackage{newtxtext,newtxmath,amsmath} % better permille symbol
\usepackage{enumitem} % handles list modifications such as \begin{itemize}[leftmargin=*]
\usepackage{subfig} % required for multiple plot arrangements
\usepackage{afterpage} % to place graphs where I want them to be more or less
\usepackage{fancyhdr}
\usepackage{hyperref} % to insert urls
\usepackage{textcomp}
\usepackage{gensymb} % degree sign
\usepackage{wasysym} % per mille sign
\usepackage{booktabs} % for fancy tables
\usepackage{multirow} % tables
\usepackage{threeparttable} % for table footnotes
\usepackage{setspace}
\usepackage{mdframed}
\usepackage[labelfont=bf]{caption} 
\usepackage{float}
\usepackage{amsfonts} % provides ticks (the \checkmark character)
\usepackage{tikz} % for organogram
\usepackage{dichokey} % for dichotomous stats key
\usepackage{lipsum} % also required for the stats key layout
\usetikzlibrary{arrows, shapes, positioning, shadows, trees}
\usepackage{framed} % for grey text boxes:
\renewenvironment{shaded}{%
  \def\FrameCommand{\fboxsep=\FrameSep \colorbox{shadecolor}}%
  \MakeFramed{\advance\hsize-\width \FrameRestore\FrameRestore}}%
 {\endMakeFramed}
\definecolor{shadecolor}{gray}{0.75}

\usepackage{titlesec}

\setcounter{secnumdepth}{4}
\titleformat{\paragraph}
{\normalfont\normalsize\bfseries}{\theparagraph}{1em}{}
\titlespacing*{\paragraph}
{0pt}{3.25ex plus 1ex minus .2ex}{1.5ex plus .2ex}

%%%%%%%%%%%%%%%%%%%%%%%%
%%% Set page margins %%%
%%%%%%%%%%%%%%%%%%%%%%%%

\usepackage[
top    = 2.75cm,
bottom = 2.50cm,
left   = 3.00cm,
right  = 3.00cm]{geometry}


%%===================================================================%%
%%% Setting the line spacing within in the 'knitr' chunks via LaTeX %%%
%%===================================================================%%

\ifdefined\knitrout
 \renewenvironment{knitrout}{\begin{spacing}{1}}{\end{spacing}} % sets the line spacing, unfortunately it has a stronger effect on the distance between the 'prompted' lines than on the R-output lines.
\else
\fi

% The following bit fixes the unequal distances between 'prompted' lines and output lines in the printed R-code
\setlength{\topsep}{0.5pt}

%% Note that, however, in certain configurations the above fix may have some side effects on typesetting of other environments.
%% This may also help to "compress" the output:

%\setlength{\parskip}{0pt}
%\setlength{\partopsep}{1pt}

%% Start of document %%%

\begin{document}
\SweaveOpts{concordance=TRUE}

%\begin{titlepage}

%%==============================================%%
%%% Inserting the useR! logo on the title page %%%
%%==============================================%%
%% \graphicspath{/Users/em8189/Documents/GLM_course/}
%\graphicspath{/Volumes/Seagate-Bac/Documents_Mac_AUT/SCIE807/Linear_and_generalised_linear_models_GLM/}
%\begin{figure}[h]
%\centering
%\includegraphics[height=1.75in]{Rlogo.png}\\
%\end{figure}
%\vspace{9ex} % inserting some blank vertical space between logo and title

%\begin{center}
%{\bf\huge \textit{SCIE807 Advanced Statistical Modelling}} \\ % \\ means line break
%\vspace{5ex}
%{\bf\LARGE Linear Models in R}\\ 
%\vspace{15ex}
%%\vspace{10ex}
%{\large Semester 1, 2020}\\
%\vspace{5ex}
%{\Large Auckland University of Technology}\\
%\vspace{10ex}
%\rm{\large Dr Martin Bader}\\
%\vspace{4ex}
%E-mail: martin.bader@aut.ac.nz \\

%\vspace{3ex}
%\end{center}
%\end{titlepage}

%%=====================%%
%%% Table of contents %%%
%%=====================%%

\tableofcontents
\thispagestyle{empty} % suppress page number for this page
%\addtocontents{toc}{~\hfill\textbf{Page}\par}
\newpage % page break


%%=================================%%
%%% Set page numbering and header %%%
%%=================================%%
\pagenumbering{arabic} % start arabic page numbering from there
\pagestyle{fancy}
\fancyhead{} % clear default, to set anew
%\fancyfoot{} % clear default
\fancyhead[LO,RE]{\textit{Two-sample and one-sample tests}}
%\fancyfoot[LO,RE]{\thepage} % page numbering

% Create new command for single and double quotes
\newcommand{\dq}[1]{``#1''}
\newcommand{\sq}[1]{`#1'}
\renewcommand{\headrulewidth}{0.0pt}
\renewcommand\floatpagefraction{0.8} %% default value: 0.5
\renewcommand\topfraction{0.8} 

% Changing the 'knitr' default settings, which were a missing prompt and '##' in front of the R results.
<<mychunck, echo=F>>=
knitr::opts_chunk$set(comment = NA, prompt = F, tidy = F, tidy.opts=list(blank=TRUE))
knitr::opts_knit$set(out.format = "latex")
# knit_theme$set() # query code chunk styles
#knit_theme$set("print")
options(width = 60, str = strOptions(strict.width = "cut")) # limit the paragraph width within code chunks, nope it is absolutely useless!!! 
color_block <- function(color) {
  function(x, options) sprintf('{\\color{%s}\\begin{verbatim}%s\\end{verbatim}}', color, x)
}
knitr::knit_hooks$set(warning = color_block('blue'), error = color_block('red'))
@

\section[Two-sample and one-sample tests]{Two-sample and one-sample tests}

One and two sample tests were the early cornerstones in statistical data analysis. Nowadays, however, our experimental designs tend to be more complex for those simple tests can handle. Nonetheless, these tests, the famous Student's \textit{t}-test in particular, provide excellent entry points into statistical modelling, since the underlying principle can be understood quite easily and they get you into the swing of understanding hypothesis testing and the interpretation of test statistics such as \textit{t}-, \textit{z}-, \textit{F}-, $\chi^{2}$-values etc. 

\subsection[The \emph{t}-statistic]{The \emph{t}-statistic}

The \emph{t}-statistic boils down to a simple signal-to-noise ratio where the difference between two group means (the signal or effect) is normalized (divided) by the pooled standard deviation of the two groups (the noise, Fig. \ref{fig:ttest}). 

\begin{align}
\textit{t} &= \frac{\text{Signal}}{\text{Noise}} = \frac{\text{Difference between means}}{\text{Pooled standard deviation}} = \frac{\bar{x}_{1} - \bar{x}_{2}}{\sqrt{\frac{s_{1}^2}{{n}_1} + \frac{s_{2}^2}{{n}_2} } }
\label{eq:t-value}
\end{align}

This test-statistic follows a symmetric, bell-shaped distribution similar to the normal distribution but with longer tails which is called Student's \textit{t}-distribution. The number of degrees of freedom ($n_{1} + n_{2}-2$) is the shape defining parameter of the \textit{t}-distribution (Fig. \ref{fig:t-dist}). 

\graphicspath{/Volumes/Seagate-Bac/Documents_Mac_AUT/Wiley_book_project}
\begin{figure}[h]
\centering
\includegraphics[height=2.5in]{t_dist_pdf_cdf.png}\\
\caption{Probability density function (PDF, \emph{left}) and cumulative distribution function (CDF, \emph{right}) of Student's \textit{t}-distribution. The probabilities given by the PDF sum up to 1 (100 \%) represented by the area under the curve. The PDF can thus be used to determine the probability of the random variable $x$ to fall within a particular range of values (continuous probability functions are defined for an infinite number of points over a continuous interval and thus the probability at a single point is always zero). The CDF gives the probability that a real-valued random variable $X$ will take on a value less than or equal to $x$ (the theoretical quantiles).} \label{fig:t-dist}
\end{figure}

\vspace{2ex}

\subsection[Two sample tests - Comparing two groups]{Two sample tests - Comparing two groups}
\subsubsection[Student's \textit{t}-test]{Student's \textit{t}-test}
Student's \emph{t}-test is applied in situations where we have a continuous response variable and two groups (two samples) to compare. The two groups represent a binary explanatory variable such as a crop fertilizer treatment, where we have a control group without fertilizer addition and a treatment group receiving the fertilizer product. The test assumes:

\begin{itemize}
\item that the data approximately follows a \textbf{\emph{normal distribution}} in each group
\item \textbf{\emph{homogeneity of variances}}, \emph{i.e.} the variance of the response variable should be equal in each group
\item \textbf{\emph{independence of observations}}, \emph{i.e.} within each group, samples are independently and randomly drawn from the population. Consequently, each observation should belong to only one group and there should be no relationship between the observations within a group.
\end{itemize}


\paragraph[Testing for normality]{Testing for normality}
We can use quantile-quantile plots or histograms as visual tools to evaluate whether the data is normally distributed (Fig. \ref{fig:qq-hist}), or, choose a formal statistical procedure in the form of one of the many available normality test, of which perhaps the most popular are:

\begin{itemize}
\item \textit{Shapiro-Wilk} test
\item \textit{Kolmogorov-Smirnov} (K-S) test
\item \textit{Anderson-Darling} test (a modified version of the K-S test)
\item \textit{Cramér-von Mises} test (a more powerful verion of the K-S test)
\end{itemize}

Additional normality tests can be found in the R package \texttt{nortest}. \\

We can either form subsets for each group and apply the graphical normality checks and formal normality tests separately or we can do it all in one-go using the functionality given in the tidyverse collection of R packages which were designed for efficient data wrangling. 

\vspace{2ex}

<<fig.show='hide', warning=F, message=F>>=
## Load required packages
library(tidyverse)
library(broom)
library(rstatix)
library(ggpubr)

## Dummy t-test data
set.seed(2)
dat <- data.frame(response = round(c(rnorm(n = 30, mean = 5), 
                                     rnorm(n = 30, mean = 8)), digits = 2), 
                  treat = rep(c("control", "treatment"), each = 30))

head(dat, n = 3)


## Group subsets
ctrl <- dat[dat$treat == "control", "response"]
trt <- dat[dat$treat == "treatment", "response"]

## Quantile-quantile plots
## Control group
qqnorm(ctrl)
qqline(ctrl)

## Treatment group
qqnorm(trt)
qqline(trt)

## Histogram
hist(ctrl, breaks = 6)
hist(trt, breaks = 5)

## Quantile-quantile plot
ggqqplot(dat, x = "response", facet.by = "treat")

## Density plot
gghistogram(data = dat, x = "response", facet.by = "treat", fill = "lightgray", 
            bins = 8, add_density = T, xlab = "Response", 
            ylab = "Probability density")
@

\graphicspath{/Volumes/Seagate-Bac/Documents_Mac_AUT/Wiley_book_project}
\begin{figure}[h]
\centering
\includegraphics[width=\textwidth]{normality.png}\\
\caption{Quantile-quantile plots (\textbf{\textit{QQ}-plot}, \emph{left}): a visual tool to assess normality by plotting a vector of random variables against the quantiles (values) of a theoretical normal distribution. If the data points are close to the quantile-quantile line, we can assume that they approximately follow a normal distribution. Histograms (\emph{right}) visualize the distribution of a continuous random variable, here overplotted with a kernel density estimate of the probability density of the data.} \label{fig:qq-hist}
\end{figure}

<<'T-tests and Wilcoxon test I', eval=F, echo=F, tidy=T, fig.width=8, fig.height=5, fig.show='hold', out.width='.45\\linewidth', fig.align='center', message=F, warning=F, fig.cap='Quantile-quantile plot (\\textbf{\\textit{QQ}-plot}): a visual tool to assess normality by plotting a vector of random variables against the quantiles (values) of a theoretical normal distribution. If the data points are close to the 1:1 line, we can assume that they approximately follow a normal distribution.\\label{fig:qqplot}'>>=

## Dummy data
set.seed(2)
dat <- data.frame(y = c(rnorm(n = 30, mean = 5), rnorm(n = 30, mean = 8)), 
                  treat = rep(letters[1:2], each = 30))

## Quantile-quantile plot
op <- par(mar = c(4, 5, 2, 3))
a <- dat[dat$treat == "a", "y"]
b <- dat[dat$treat == "b", "y"]

## Group a
qqnorm(a, pch = 21, bg = "black", col = "white", lwd = 0.6, las = 1, tcl = -0.35, mgp = c(3, 0.9, 0), cex.axis = 1.5, cex = 1.5, cex.lab = 1.5, cex.main = 1.8)
qqline(a, col = "white", lwd = 2)
qqline(a)

## Group b
qqnorm(b, pch = 21, bg = "black", col = "white", lwd = 0.6, las = 1, tcl = -0.35, mgp = c(3, 0.9, 0), cex.axis = 1.5, cex = 1.5, cex.lab = 1.5, cex.main = 1.8)
qqline(b, col = "white", lwd = 2)
qqline(b)


## Histogram
hist(a, main = NA, freq = F, ann = F, las = 1, tcl = -0.35, mgp = c(2, 0.9, 0), cex.axis = 1.5, cex = 1.5)
library(KernSmooth)
smth <- bkde(x = a, bandwidth = dpik(a), range.x = c(2, 11), canonical = T)
lines(smth, col = "red2")
mtext("Density", side = 2, line = 4, cex = 1.5)
mtext("Random variable", side = 1, line = 2.5, cex = 1.5)

hist(b, main = NA, freq = F, ann = F, las = 1, tcl = -0.35, mgp = c(2, 0.9, 0), cex.axis = 1.5, cex = 1.5)
library(KernSmooth)
smth <- bkde(x = b, bandwidth = dpik(b), range.x = c(6, 10.5), canonical = T)
lines(smth, col = "red2")
mtext("Density", side = 2, line = 4, cex = 1.5)
mtext("Random variable", side = 1, line = 2.5, cex = 1.5)
par(op)
@

\vspace{2ex}

The null hypothesis underlying the normality tests states that the distribution of our sample data is not signficantly different from a normal distribution and therefore we expect the resulting \textit{P}-value to be \emph{larger} than 0.05, if our data really follows a normal distribution. This is in contrast to the majority of analysis scenarios down the road, where we often expect a significant effect and thus a \textit{P}-value smaller than 0.05 prompting us to reject the null hypothesis and accept the alternative hypothesis (there is an effect). Consequently, a significant outcome of a normality test indicates that the tested data follows a distribution other than the normal distribution.\\*

The \textbf{\textit{Shapiro-Wilk}} test is perhaps the best known normality test. The test is performed using the \texttt{shapiro.test} function, which only requires a single input vector holding the data to be tested for normality. 

\vspace{2ex}

<<shapiro>>=
## Shapiro-Wilk normality test
shapiro.test(ctrl)
shapiro.test(trt)
@

\vspace{2ex}

Similar to the graphical exploration above, we can use a tidyverse approach and for the Shapiro-Wilk test there is even the \texttt{shapiro\_test} wrapper around the base function \texttt{shapiro.test} making it easy to use when coding with grouped data and pipes (recognisable by the pipe operator \texttt{\%>\%}). 

\vspace{2ex}

<<shapiro tidy>>=
## Shapiro-Wilk normality test
group_by(.data = dat, treat) %>% shapiro_test(response)
@

\vspace{2ex}

The \textbf{\textit{Kolmogorov-Smirnov}} (\textbf{K-S}) test compares the distribution of a set of random variables to a user-chosen reference distribution (one-sample K-S test). Since the test compares the empirical distribution function of the sample to the cumulative distribution function of a reference distribution and because the cumulative distribution functions in R all start with `p' (\emph{e.g.} \texttt{pnorm}, \texttt{ppois}, \texttt{pgamma}, etc.), we need to specify `pnorm' in the \texttt{ks.test} function. Alternatively, we can use the K-S test to compare two sets of random variables to test whether they share the same distribution (two-sample K-S test).

\vspace{2ex}

<<kstestI, message=F, warning=F>>=
## Kolomogorov-Smirnov test
##  One-sample test comparing the input data to a 
## theoretical normal distribution given the same 
## mean and standard deviation

## Old school coding
ks.test(x = ctrl, y = "pnorm", mean = mean(ctrl), sd = sd(ctrl))
ks.test(x = trt, y = "pnorm", mean = mean(trt), sd = sd(trt))
ks.test(ctrl, trt)

## tidyverse coding
group_by(.data = dat, treat) %>% 
  group_modify(.f = ~ tidy(ks.test(x = .x$response, y = "pnorm", 
                                   mean = mean(.x$response), 
                                   sd = sd(.x$response))))
@

\vspace{2ex}

<<kstestII, eval=F, echo=F, warning=F, message=F>>=
# Two-sample test comparing x to a simulated normal distribution with the
# same mean and standard deviation
ks.test(x = dat$y, y = rnorm(100, mean = mean(dat$y), sd = sd(dat$y)))
ks.test(x = dat$y, y = rpois(100, lambda = 7))
@

\vspace{2ex}

The \textbf{\textit{Anderson-Darling}} test is a modified, more powerful version of the K-S test giving more weight to the tails, which can be performed using the function \texttt{ad.test} (R package \texttt{nortest}). 

\vspace{2ex}

<<Anderson-Darling test>>=
## Anderson-Darling test
library(nortest)

## Old school coding
ad.test(ctrl)
ad.test(trt)

## tidyverse coding
group_by(.data = dat, treat) %>% 
  group_modify(.f = ~ tidy(ad.test(.x$response)))
@

\vspace{2ex}

Similarly, the \textbf{\textit{Cramér-von Mises}} test is a more powerful version of the K-S test which can be run with the command \texttt{cvm.test} (R package \texttt{nortest}).

\vspace{2ex}

<<Cramér-von Mises test>>=
## Cramér-von Mises test
## Old school coding
cvm.test(ctrl)
cvm.test(trt)

## tidyverse coding
group_by(.data = dat, treat) %>% 
  group_modify(.f = ~ tidy(cvm.test(.x$response)))
@

\vspace{2ex}

Apart from normally distributed data, Student's \textit{t}-test assumes equal variances among the two groups to be compared. This so-called \textit{variance homogeneity} criterion can be formally tested using a range of variance tests such as Levene's test (\texttt{levene\_test}, R package \emph{rstatix}), Barthlett's test (\texttt{bartlett.test}, built-in) or the Fligner-Killeen test, (\texttt{fligner.test}, built-in) to name but a few. Similar to the normality tests, the null hypothesis of variance homogeneity tests assumes no difference in group variances and thus a statistically significant test results indicates unequal variances among groups.  

\vspace{2ex}

<<Variance homogeneity, warning=F, message=F>>=
## Checking for equal variances
levene_test(data = dat, formula = response ~ treat)
@

\vspace{2ex}

In case of equal group variances, the pooled variance is used as overall variance estimate. However, if this assumption is violated, then the variance needs to be estimated separately for each group, which involves a modification to the degrees of freedom, known as \textit{Welch approximation}. Since Welch's \textit{t}-test version is superior to Student's \textit{t}-test when sample sizes and variances are unequal between groups and yields the same result when sample sizes and variances are equal, the built-in \texttt{t.test} function uses the Welch approximation by default, so testing for equal variances in connection with a \emph{t}-test is actually redundant. This default setting is implemented by the \texttt{var.equal = FALSE} argument (the default setting). When set to \texttt{var.equal = TRUE}, the \texttt{t.test} function assumes equal group variances and computes the original Student's \textit{t}-test. \\*

\noindent As we have seen above, the \textit{t}-statistic is easily computed as the difference between group means divided by the the pooled standard deviation (as a measure of spread), which can be regarded as a simple signal-to-noise ratio (signal: difference between means, noise: pooled standard deviation; Fig. \ref{fig:ttest}). The test procedure then queries the probability density function underlying the \textit{t}-distribution to obtain an answer to the question: \\*

\emph{How likely is it to obtain a \textit{t}-value as extreme as the observed one just by chance?}\footnote{\noindent Note, the use of the term 'extreme' instead of 'large' to account for the fact that the sign of the test statistic can be negative or positive. One could rephrase the question as: How likely is it to obtain a \textit{t}-value as large as the absolute value of the observed one, just by chance?} \\*

\noindent We then compare the resulting probability, the so-called \textit{P}-value, to the probability of the critical \emph{t}-value corresponding to our preset significance level of 5 \% (by convention denoted by $\alpha = 0.05$). If our \textit{P}-value is smaller than 0.05, we reject the null hypothesis of no difference among the two groups in favour of the alternative hypothesis and conclude that there is a statistically significant difference between the two groups.

\begin{figure}[h]
\centering
\includegraphics[height=6in]{t_test.png}\\
\caption{Three different \textit{t}-test scenarios with identical means but increasing amount of spread (greater standard deviation) from top to bottom. Note the reduction of the \emph{t}-statistic in response to the increasing standard deviation (increase in noise).} \label{fig:ttest}
\end{figure}

\vspace{2ex}

The \textit{t}-test can be specified using a formula interface (in our view this is the preferred option since more sophisticated models all rely on it) or using a simple $x, y$ notation.

\vspace{2ex}

<<ttest, echo=1:5, eval=1:5>>=
## Welch's t-test, formula interface
t.test(response ~ treat, data = dat)

## Traditional interface using 'x, y' notation and the previously created subsets
t.test(x = ctrl, y = trt)
@

<<t-value calculation by hand, eval=F, echo=F>>=
(mean(dat[dat$treat == "control", "response"]) - mean(dat[dat$treat == "treatment", "response"]))/(sqrt((sd(dat[dat$treat == "control", "response"])^2*29 + sd(dat[dat$treat == "treatment", "response"])^2*29)/58) * sqrt(1/30 + 1/30))
@

\vspace{2ex}
\paragraph[What to write in a report or paper and how to visualise the results of a \emph{t}-test]{What to write in a report or paper and how to visualise the results of a \emph{t}-test}

We can wrap up the results in one concise sentence like this: \newline
The treatment caused a statistically significant increase in the response by 52 \% compared to the control (\emph{t} = -9.13, \emph{df} = 57.97, \emph{P} < 0.001).\\*

The results are best displayed as bar- or boxplots. Since the \emph{t}-test relies on means rather than the median (quartiles) like boxplots, the classical barplot is our favourite option (Fig. \ref{fig:barplot_boxplot_t_test}). First, we need to aggregate our data, \emph{i.e.} compute the group means and the associated standard errors.

\vspace{2ex}

<<barplot, warning=F, message=F, echo=-1, eval=F>>=
source('~/Documents/R_custom_functions/standard_error.R', encoding = 'UTF-8')
## Aggregate the data for plotting
dat2 <- group_by(dat, treat) %>% summarise(response_mean = mean(response), 
                                           pos = mean(response) + se(response), 
                                           neg = mean(response) - se(response))
bp <- barplot(dat2$response_mean, las = 1, ylim = c(0, 10), 
              col = c("white", "grey"))
bp # midpoint of the bars

arrows(x0 = bp, y0 = dat2$neg, x1 = bp, y1 = dat2$pos, length = 0.05, 
       angle = 90, code = 3)
text(x = bp, y = dat2$pos, labels = c("a", "b"), pos = 3)
mtext(text = "Response", side = 2, line = 2.5, cex = 0.9)
mtext(text = c("Control", "Treatment"), side = 1, line = 0.5, 
      cex = 0.9, at = bp)
@

\vspace{2ex}

\graphicspath{/Volumes/Seagate-Bac/Documents_Mac_AUT/Wiley_book_project}
\begin{figure}[h]
\centering
\includegraphics[width=0.7\textwidth]{barplot_boxplot_t_test.png}\\
\caption{Visualization options for \emph{t}-test results. The classical barplot (\emph{left}) shows the means $\pm$ standard errors, whereas a boxplot displays the minimum and maximum indicated by the end of the whiskers, the 1st and 3rd quartile given by the hinges of the box, and the median denoted by the thick line inside the box. C = Control, T = Treatment} \label{fig:barplot_boxplot_t_test}
\end{figure}

\vspace{2ex}
%We can easily check the validity of the \textit{P}-value by hand taking the \emph{t}-value and the degrees of freedom from the \emph{t}-test summary and plugging them into the cumulative probability distribution function of the \texit{t}-distribution which goes by the R command \texttt{pt} ('p' stands for probability). By default, the \emph{t}-test leaves the 'direction' of the alternative hypothesis open, \emph{i.e.} if there is a statistically significant difference between the two groups, then the second group's mean could be significantly higher or lower than the mean of the first group and therefore we need multiply the outcome of the \emph{pt} function by two.

\vspace{2ex}
<<Calculate P-value by hand, eval=F, echo=F>>=
pt(q = -9.1316, df = 57.97, lower.tail = T) * 2
@

\vspace{2ex}

\paragraph[Two-tailed \emph{vs.} one-tailed \emph{t}-tests]{Two-tailed \emph{vs.} one-tailed \emph{t}-tests}
The \emph{t}-test allows us to specify the alternative hypothesis via the \texttt{alternative} argument, whose default is `two-sided', translating into the alternative hypothesis: `the difference between group means is not zero'. This means that we make no \emph{a priori} assumption as to whether the mean of group one ($\bar{x}_1$) is smaller or greater than the mean of group two ($\bar{x}_2$). So, the resulting \emph{t}-value can be positive ($\bar{x}_1 > \bar{x}_2$) or negative ($\bar{x}_1 < \bar{x}_2$), and to account for this, the 5 \% \emph{type I error} probability is divided between the two tails of the distribution. Setting the \texttt{alternative} argument to `less', results in a one-tailed test assuming that the mean of the first group is smaller than the mean of the second group and \emph{vice versa} when `greater' is specified (Fig. \ref{fig:two-tailed vs one-tailed}). One-tailed \emph{t}-tests should only be applied if we have very strong reason to believe that the difference can only go in one particular direction. Shifting the 5 \% error probability all in one tail reduces the critical \emph{t}-value and therefore one-tailed tests have more statistical power to detect an effect in one particular direction than a two-tailed test applied to the same data.

\vspace{2ex}

\graphicspath{/Volumes/Seagate-Bac/Documents_Mac_AUT/Wiley_book_project}
\begin{figure}[h]
\centering
\includegraphics[width=6in]{two-sided_one-sided_t_test.png}\\
\caption{Two-tailed \emph{vs.} one-tailed \emph{t}-tests. By default the \texttt{t.test} function runs as a two-tailed test meaning that we make no assumptions as to which group mean is larger. Consequently, the \emph{t}-statistic may be positive ($\bar{x}_1 > \bar{x}_2$) or negative ($\bar{x}_1 < \bar{x}_2$), \emph{i.e.} it may sit in the left or right tail of the distribution and in order to take this into account, we need to split our 5 \% \emph{type I error} probability, given by the significance level ($\alpha = 0.05$), in two and allocate one half to the left tail and the other half to the right tail. If we have strong reason to believe that the mean of group 2 can only be larger than that of group 1 ($\bar{x}_1 < \bar{x}_2$), then we can shift the whole 5 \% error probability into the left tail resulting in a one-tailed \emph{t}-test (middle panel). In the opposite case, when we strongly assume that $\bar{x}_1 > \bar{x}_2$, we shift the whole 5 \% error probability into the right-hand tail (right panel). Note the lower critical \emph{t}-values associated with shifting the whole 5 \% error probability into one tail, which makes a one-tailed \emph{t}-test more powerful than a two-tailed one for detecting changes in one particular direction. The shape of the \emph{t}-distribution and the critical \emph{t}-values ($t_{crit}$) are based on a hypothetical example with 10 observations per group and thus $n_{1} + n_{2} - 2 = 18$ degrees of freedom.} \label{fig:two-tailed vs one-tailed}
\end{figure}

\vspace{2ex}

<<'T-test II'>>=
## One-sided t-test

# H0: first mean is smaller than the second
t.test(response ~ treat, data = dat, alternative = "less")

# H0: first mean is larger than the second
t.test(response ~ treat, data = dat, alternative = "greater") 
@

\vspace{2ex}

In `before and after' experiments, \textit{e.g.} when the efficacy of a treatment is tested on the same subjects, the observations are not independent, which commonly results in less variation compared to data derived from studies contrasting independent groups. In those situations, a paired \textit{t}-test is required to account for the dependence among observations and this test mode can be implemented by setting \texttt{paired = TRUE}. Consider the following small dataset on depression scale measurements in nine patients taken at the first (1) and second (2) visit after initiation of a therapy (administration of a tranquillizer). 

\vspace{2ex}

<<Paired t-test>>=
depression <- data.frame(score = c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55,
                                   3.06, 1.30, 0.83, 0.61, 0.57, 2.03, 1.06, 
                                   1.29, 1.06, 3.14, 1.29),
                         visit = rep(1:2, each = 9), patient = 1:9)
t.test(score ~ visit, data = depression, paired = T)
t.test
@

\vspace{2ex}

\noindent So, what is the magic behind a paired \emph{t}-test? \newline

\noindent In order to generate an independent dataset, the paired \emph{t}-test computes the differences between before and after observations of each patient. With this little trick we get rid of the dependency. In the original dataset, we had two observations per patient (before and after) but using the differences leaves us with only one observation per patient and makes this an independent dataset. We can now test the before-after differences against the null hypothesis that the mean difference is zero. This sort of analysis is done using a one-sample \emph{t}-test and this is what goes on behind the scenes of a paired \emph{t}-test: it runs a one-sample \emph{t}-test on the differences. We will learn about one-sample \emph{t}-tests below.

\vspace{2ex}

\subsubsection[Rank based two sample tests]{Rank based two sample tests}
The \textbf{\emph{Wilcoxon Rank Sum test}} is a non-parametric alternative to the \textit{t}-test. Non-parametric tests do not assume an underlying probability distribution of the sample data, meaning that our data does not need to be normally distributed. The test procedure relies on ranks rather than on the actual values and thus makes it very robust against outliers (which do have a strong influence on the \emph{t}-test though). 

\vspace{2ex}

<<Ranks>>=
a <- c(2, 3, 5); b <- c(3, 6, 8)
x <- c(0, 3, 4); y <- c(3, 23, 700)
rank(c(a, b))
rank(c(x, y))
@

\vspace{2ex}

The test is implemented by the \texttt{wilcox.test} function, which also features a \texttt{paired} argument, allowing the analysis of paired designs (dependent observations). In the unpaired case (independent samples), we talk about the \textit{Wilcoxon rank sum} test, which is equivalent to the \textit{Mann-Whitney} test (a.k.a. U-test). In the paired case (dependent samples), we refer to this procedure as the \emph{Wilcoxon signed rank} test.

\vspace{2ex}

<<Wilcoxon test I, warning=F, message=F>>=
## Formula notation
wilcox.test(response ~ treat, data = dat)

## Traditional interface using 'x, y' notation
wilcox.test(x = ctrl, y = trt)
@

\vspace{2ex}

In the same manner as the \emph{t}-test, the Wilcoxon test can be run as a one-tailed test or a paired test, e.g. the analysis of the 

\vspace{2ex}

<<Wilcoxon test II, warning=F, message=F>>=
wilcox.test(score ~ visit, data = depression, paired = T)
@

\vspace{2ex}

\subsection[One-sample tests]{One-sample tests}

At first glance, a one-sample test seems paradoxical but sometimes we simply want to compare a sample mean with a known value, \emph{e.g.} when we would like to test whether a fold-change is significantly different from 1 or when we want to compare the mean of a randomly chosen sample to a known population mean. In those cases, a one-sample \emph{t}-test or a one-sample \emph{Wilcoxon} test can be applied by providing the observations of the sample and a single value for the population mean instead of the observations from a second group. Imagine a crop scientist who was interested in determining whether priming of wheat seeds with silica resulted in a larger average height of wheat seedlings than the standard height of 10.5 cm. The researcher treated a random sample of $n = 33$ seeds with silica nanoparticles prior to sowing and subsequently obtained the seedling heights in cm given in the `seeds' vector below. 

\vspace{2ex}

<<'One-sample tests', warning=F, message=F>>=
## Dummy data
seeds <- c(11.5, 11.8, 15.7, 16.1, 14.1, 10.5, 15.2, 19.0, 12.8, 12.4, 19.2, 
           13.5, 16.5, 13.5, 14.4, 16.7, 10.9, 13.0, 15.1, 17.1, 13.3, 12.4, 
           8.5, 14.3, 12.9, 11.1, 15.0, 13.3, 15.8, 13.5, 9.3, 12.2, 10.3)
mean(seeds)

## The mean is 13.7 cm but is this a significant increase compared to 
## the 10.5 cm standard height?
t.test(seeds, mu = 10.5)

wilcox.test(seeds, mu = 10.5)
@

\vspace{2ex}

Both one-sample tests give us a significant result indicating that silica seed priming significantly increases seedling height by 30 \% (\texttt{mean(seeds)/10.5 - 1}).

\vspace{2ex}

At this point, we will revisit the `depression' example to unveil the inner workings of a paired \emph{t}-test. Let us calculate the difference in depression scores between the first and the second visit and analyse it with a one-sample \emph{t}-test.

\vspace{2ex}

<<>>=
difference <- depression[depression$visit == "1", "score"] - 
              depression[depression$visit == "2", "score"]
t.test(difference, mu = 0)
@

\vspace{2ex}

When we compare the outcome of the one-sample \emph{t}-test applied to the differences in depression scores to a paired \emph{t}-test using the original data, we can see that they yield identical results, which shows that a paired \emph{t}-test boils down to a one-sample test with the null hypothesis that the difference between the mean of the sample and a reference mean is zero.

\vspace{2ex}

<<>>=
t.test(score ~ visit, data = depression, paired = T)
@

\vspace{2ex}

\subsection[Power analyses and sample size determination]{Power analyses and sample size determination}
The power of a hypothesis test is the probability of correctly rejecting the null hypothesis ($H_{0}$) when the alternative hypothesis ($H_{A}$) is true (Fig. \ref{fig:power}). In other words: the power indicates the probability of avoiding a type II error ($\beta$; a \emph{false negative}, failing to reject a false null hypothesis) and is thus defined as $1 - \beta$ (hatched area in Fig. \ref{fig:power}). Like every probability, the power of a test ranges from 0 to 1 and with increasing statistical power, the probability of committing a type II error decreases. There are four factors affecting the power of a statistical test:

\begin{itemize}
\item the \textbf{sample size} \emph{n}
\item the \textbf{significance level} ($\alpha$). The lower $\alpha$, the lower the power of the test. Reducing $\alpha$ (\emph{e.g.} from 0.05 to 0.01) widens the region of acceptance and as a result, we are less likely to reject $H_{0}$ even when it is false, so we are more likely to commit a type II error ($\beta$).
\item the \textbf{effect size}. The bigger the effect size, the greater the power of the test. In a \emph{t}-test, this boils down to the \emph{t}-value. 
\item the \textbf{standard deviation} of the samples
\end{itemize}

\vspace{2ex}
\vspace{2ex}

\graphicspath{/Volumes/Seagate-Bac/Documents_Mac_AUT/Wiley_book_project}
\begin{figure}[h]
\centering
\includegraphics[height=2.5in]{power.png}\\
\caption{Type I error ($\alpha$, teal areas), type II error ($\beta$, orange area) and statistical power ($1 - \beta$, hatched area) of a two-sample, two-tailed \emph{t}-test. The dotted lines indicate the critical \emph{t}-values beyond which the test rejects the null hypothesis (no difference in means). Imagine there is a true difference in means between the two groups, but purely by chance our random sample for group 2 falls largely into the orange range of possible population values. Due to this sampling bias, we underestimate the mean of population 2 and thus the effect size which leads to a smaller \emph{t}-statistic than the critical value. In this scenario, we would commit a type II error (\emph{false negative}) because we were unable to reject the false null hypothesis yielding a non-significant \emph{t}-test result. Conversely, if no true difference in group means existed, but purely by chance our random sample of group 2 would largely consist of values to the right of the orange area, then we would overestimate the effect size and commit a type I error, \emph{i.e.} falsely rejecting the null hypothesis and concluding that there is a statistically significant difference between group means. $\bar{x}_{1}$ = mean of group 1, $\bar{x}_{2}$ = mean of group 2.} \label{fig:power}
\end{figure}

\vspace{2ex}

Before you start an experiment or an observational study, the fundamental question of how many samples to take will arise. Because of the above dependencies, the answer to this question requires an estimate of i) the variation in your population and ii) the expected difference between treatments or groups (effect size). To get estimates for these metrics, you need to perform a literature search or conduct a pilot study. Determining reasonable estimates is a good time investment: think of a situation where an expensive experiment is conducted and afterwards it is found that given the variability in the data, the expected difference could never have been detected. For example, if your expected difference is 1 (arbitrary units), the mean standard deviation in your samples is 3 and your sample size 10, then your power is as low as 10 \%! In other words, you only have a 10 \% chance of detecting a potential difference of 1 between your groups! In this case, you would have to increase your sample size dramatically to about 150 to reach a power of around 80 \%.\\*

We can use the versatile \texttt{power.t.test} function to juggle with sample size, significance level, power, standard deviation, and the expected difference between groups to help guide our experimental design. Whatever single parameter we do not specify in this function will be determined from the others and returned in the output. For example, if we provide the sample size, the estimated difference between means, the standard deviation and the significance level, then the function returns the power of the resulting \emph{t}-test. By convention, we aim at a target power of 80 \% (0.8), so we can plug this value into the function along with all other required arguments apart from one parameter of interest we would like to get an estimate for. This is great for sample size determination which can be derived by omitting the \texttt{n} argument. By default the \texttt{power.t.test} function assumes a significance level of 0.05 (argument \texttt{sig.level}), a two-sample test (arg. \texttt{type}) with a two.sided alternative hypothesis (arg. \texttt{alternative}) and therefore these arguments need not be specified unless we wish to change them.\\*

Let us start with a simple power calculation to determine whether it is worthwhile conducting a planned experiment. Say we expect a difference in means of 1 unit (specified via the \texttt{delta} argument), anticipate a standard deviation of 2 and can afford 10 samples per group.

\vspace{2ex}

<<Power test I, message=F, warning=F>>=
power.t.test(n = 10, delta = 1, sd = 2)
@

\vspace{2ex}

In this scenario, we would have a power of 18.4 \% to detect a group difference if it really existed. Given this low probability, we would either abandon our research plans or increase the sample size to gain more power. But how many samples are enough? To answer this question, we set the desired power to 80 \% and rerun the function without specifying \emph{n}.

\vspace{2ex}

<<Power test II, message=F, warning=F>>=
power.t.test(delta = 1, sd = 2, power = 0.8)
@

\vspace{2ex}

The output tells us that we need 64 samples per group to raise the power to 80 \%. 

\vspace{2ex}
\vspace{2ex}
\vspace{2ex}
\vspace{2ex}
\vspace{2ex}
\vspace{2ex}
\vspace{2ex}
\vspace{2ex}
\end{document}


