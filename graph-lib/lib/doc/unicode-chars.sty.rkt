#lang at-exp racket
(provide unicode-chars)
(define unicode-chars
  @string-append|<<<{
\makeatletter
% Must be loaded after MnSymbol!!! MnSymbol improperly defines × and ¬ in such a
%   way that they don't work in math mode.
% definition of some characters, for use with
% \usepackage[utf8]{inputenc}
% \usepackage[T1]{fontenc}
% Author: Christoph Lange <math.semantic.web@gmail.com>
% Some math characters taken from John Wickerson's MathUnicode.sty
%  (http://tex.stackexchange.com/questions/110042/
%     entering-unicode-math-symbols-into-latex-direct-from-keyboard-on-a-mac)
% https://github.com/clange/latex
\NeedsTeXFormat{LaTeX2e}[1999/12/01]
\ProvidesPackage{unicode-chars}[2013/10/08]

%\DeclareUnicodeCharacter{00A0}{~}%                                  
%\DeclareUnicodeCharacter{00A3}{\pounds}%                          £
%\DeclareUnicodeCharacter{00AC}{\ensuremath{\neg}}                 ¬
%\DeclareUnicodeCharacter{00AE}{\textsuperscript{\textregistered}}%®
%\DeclareUnicodeCharacter{00AF}{\ensuremath{^-}}%                  ¯
%\DeclareUnicodeCharacter{00D7}{\ensuremath{\times}}%              ×

%%%%%%%%%%%%%%%%%%%%%%%%% vvv % NO-BREAK SPACE here (unicode 00A0)
\catcode`\^^a0=13\relax\def {~}%                                   " " (nbsp)
\catcode`\^^a3=13\relax\def£{\pounds}%                             £
\catcode`\^^ae=13\relax\def®{\textsuperscript{\textregistered}}%   ®
% macron: overline, overbar
\catcode`\^^af=13\relax\def¯{\ensuremath{^-}}%                     ¯ % macron
% \catcode`\^^f1=13\relax\defñ{\~{n}}%                               ñ
% Declared by MnSymbol:
% \catcode`\^^d7=13\relax\def×{\ensuremath{\times}}%                 ×
% \catcode`\^^ac=13\relax\def¬{\ensuremath{\neg}}\relax%               ¬

\DeclareUnicodeCharacter{00F1}{{\ifmmode\tilde{n}\else\~{n}\fi}}
% Declared by MnSymbol:
\DeclareUnicodeCharacter{00D7}{\ensuremath{\times}}
\DeclareUnicodeCharacter{00AC}{\ensuremath{\neg}}

\DeclareUnicodeCharacter{0101}{\=a}%                               ā
\DeclareUnicodeCharacter{0123}{\c g}%                              ģ
\DeclareUnicodeCharacter{0130}{\. I}%                              İ
\DeclareUnicodeCharacter{0146}{\c n}%                              ņ
\DeclareUnicodeCharacter{016B}{\=u}%                               ū
\DeclareUnicodeCharacter{03B1}{\ensuremath{\alpha}}%               α
\DeclareUnicodeCharacter{03B4}{\ensuremath{\delta}}%               δ
\DeclareUnicodeCharacter{0394}{\ensuremath{\Delta}}%               Δ
\DeclareUnicodeCharacter{03F5}{\ensuremath{\epsilon}}%             ϵ
\DeclareUnicodeCharacter{03B5}{\ensuremath{\varepsilon}}%          ε
\DeclareUnicodeCharacter{0395}{\ensuremath{\Epsilon}}%             Ε
\DeclareUnicodeCharacter{03BB}{\ensuremath{\lambda}}%              λ
\DeclareUnicodeCharacter{03C1}{\ensuremath{\rho}}%                 ρ
\DeclareUnicodeCharacter{03A1}{\ensuremath{\Rho}}%                 Ρ
\DeclareUnicodeCharacter{2190}{\ensuremath{\leftarrow}}%           ←
\DeclareUnicodeCharacter{2192}{\ensuremath{\rightarrow}}%          →
% 2192: \textrightarrow is not available in all fonts,
% and we need the right arrow in math mode
\DeclareUnicodeCharacter{2193}{\ensuremath{\downarrow}}%           ↓
\DeclareUnicodeCharacter{2194}{\ensuremath{\leftrightarrow}}%      ↔
\DeclareUnicodeCharacter{21A6}{\ensuremath{\mapsto}}%              ↦
\DeclareUnicodeCharacter{21C0}{\ensuremath{\rightharpoonup}}%      ⇀
\DeclareUnicodeCharacter{21D2}{\ensuremath{\Rightarrow}}%          ⇒
% Georges — added \operatorname{} in ∀ .
\DeclareUnicodeCharacter{2200}{\ensuremath{\operatorname{\forall}}}% ∀
\DeclareUnicodeCharacter{2203}{\ensuremath{\exists}}%              ∃
\DeclareUnicodeCharacter{2208}{\ensuremath{\in}}%                  ∈
\DeclareUnicodeCharacter{2209}{\ensuremath{\not\in}}%              ∉
\DeclareUnicodeCharacter{2211}{\ensuremath{\sum}}%                 ∑
\DeclareUnicodeCharacter{220F}{\ensuremath{\prod}}%                ∏
\DeclareUnicodeCharacter{2218}{\ensuremath{\circ}}%                ∘
\DeclareUnicodeCharacter{2227}{\ensuremath{\mathbin{\wedge}}}%               ∧
\DeclareUnicodeCharacter{2228}{\ensuremath{\mathbin{\vee}}}%                 ∨
\DeclareUnicodeCharacter{2229}{\ensuremath{\mathbin{\cap}}}%                 ∩
\DeclareUnicodeCharacter{222A}{\ensuremath{\mathbin{\cup}}}%                 ∪
\DeclareUnicodeCharacter{228D}{\ensuremath{\mathbin{\cupdot}}}%              ⊍
\DeclareUnicodeCharacter{228E}{\ensuremath{\mathbin{\uplus}}}%               ⊎
%\DeclareUnicodeCharacter{2237}{\ensuremath{::}}%                   ∷
% 2237: not sure that's a good way to render this symbol
\DeclareUnicodeCharacter{2248}{\ensuremath{\approx}}%              ≈
\DeclareUnicodeCharacter{2260}{\ensuremath{\ne}}%                  ≠
\DeclareUnicodeCharacter{2261}{\ensuremath{\equiv}}%               ≡
\DeclareUnicodeCharacter{2264}{\ensuremath{\le}}%                  ≤
\DeclareUnicodeCharacter{2265}{\ensuremath{\ge}}%                  ≥
\DeclareUnicodeCharacter{2286}{\ensuremath{\subseteq}}%            ⊆
\DeclareUnicodeCharacter{2287}{\ensuremath{\supseteq}}%            ⊇
\DeclareUnicodeCharacter{219D}{\ensuremath{\leadsto}}%             ↝
\@ifpackageloaded{MnSymbol}{%
\DeclareUnicodeCharacter{2295}{\ensuremath{\oplus}}%               ⊕
\DeclareUnicodeCharacter{2296}{\ensuremath{\ominus}}%              ⊖
}{}
\DeclareUnicodeCharacter{22C0}{\ensuremath{\bigwedge}}%            ⋀ 
\DeclareUnicodeCharacter{22C0}{\ensuremath{\bigcupdot}}%           ⋀
\DeclareUnicodeCharacter{22C1}{\ensuremath{\biguplus}}%            ⋁
\DeclareUnicodeCharacter{22C2}{\ensuremath{\bigcap}}%              ⋂
\DeclareUnicodeCharacter{22C3}{\ensuremath{\bigcup}}%              ⋃
\DeclareUnicodeCharacter{2A03}{\ensuremath{\bigcupdot}}%           ⨃
\DeclareUnicodeCharacter{2A04}{\ensuremath{\biguplus}}%            ⨄
\DeclareUnicodeCharacter{25CB}{\ensuremath{\ocircle}}%             ○
\@ifpackageloaded{MnSymbol}{%
\DeclareUnicodeCharacter{2605}{\ensuremath{\filledlargestar}}%     ★
}{}
\DeclareUnicodeCharacter{2713}{\ensuremath{\checkmark}}%           ✓
\DeclareUnicodeCharacter{27F6}{\ensuremath{\longrightarrow}}%      ⟶
\DeclareUnicodeCharacter{27F7}{\ensuremath{\longleftrightarrow}}%  ⟷
\DeclareUnicodeCharacter{27F9}{\ensuremath{\Longrightarrow}}%      ⟹
%
% Additions by Georges Dupéron
\DeclareUnicodeCharacter{2237}{\ensuremath{\dblcolon}}%            ∷
\DeclareUnicodeCharacter{228F}{\ensuremath{\sqsubset}}%            ⊏
\DeclareUnicodeCharacter{2290}{\ensuremath{\sqsubset}}%            ⊐
\DeclareUnicodeCharacter{2291}{\ensuremath{\sqsubseteq}}%          ⊑
\DeclareUnicodeCharacter{2292}{\ensuremath{\sqsupseteq}}%          ⊒
\DeclareUnicodeCharacter{2293}{\ensuremath{\sqcap}}%               ⊓
\DeclareUnicodeCharacter{2294}{\ensuremath{\sqcup}}%               ⊔
%
\usepackage{graphicx}%
\providecommand{\bigsqcap}{%
  \mathop{%
    \mathpalette\@updown\bigsqcup
  }%
}
\newcommand*{\@updown}[2]{%
  \rotatebox[origin=c]{180}{$\m@th#1#2$}%
}
\DeclareUnicodeCharacter{2A05}{\ensuremath{\bigsqcap}}%            ⨅
\DeclareUnicodeCharacter{2A06}{\ensuremath{\bigsqcup}}%            ⨆
\DeclareUnicodeCharacter{2080}{\ensuremath{_0}}%                   ₀
\DeclareUnicodeCharacter{2081}{\ensuremath{_1}}%                   ₁
\DeclareUnicodeCharacter{2082}{\ensuremath{_2}}%                   ₂
\DeclareUnicodeCharacter{2083}{\ensuremath{_3}}%                   ₃
\DeclareUnicodeCharacter{2084}{\ensuremath{_4}}%                   ₄
\DeclareUnicodeCharacter{2085}{\ensuremath{_5}}%                   ₅
\DeclareUnicodeCharacter{2086}{\ensuremath{_6}}%                   ₆
\DeclareUnicodeCharacter{2087}{\ensuremath{_7}}%                   ₇
\DeclareUnicodeCharacter{2088}{\ensuremath{_8}}%                   ₈
\DeclareUnicodeCharacter{2089}{\ensuremath{_9}}%                   ₉
\DeclareUnicodeCharacter{208A}{\ensuremath{_+}}%                   ₊
\DeclareUnicodeCharacter{208B}{\ensuremath{_-}}%                   ₋
\DeclareUnicodeCharacter{208C}{\ensuremath{_=}}%                   ₌
\DeclareUnicodeCharacter{208D}{\ensuremath{_(}}%                   ₍
\DeclareUnicodeCharacter{208E}{\ensuremath{_)}}%                   ₎
\DeclareUnicodeCharacter{2098}{\ensuremath{_m}}%                   ₘ
\DeclareUnicodeCharacter{2099}{\ensuremath{_n}}%                   ₙ
\DeclareUnicodeCharacter{1D62}{\ensuremath{_i}}%                   ᵢ
\DeclareUnicodeCharacter{2C7C}{\ensuremath{_j}}%                   ⱼ
%
\DeclareUnicodeCharacter{2070}{\ensuremath{^0}}%                   ⁰
%\DeclareUnicodeCharacter{00B9}{\ensuremath{^1}}%                   ¹
%\DeclareUnicodeCharacter{00B2}{\ensuremath{^2}}%                   ²
%\DeclareUnicodeCharacter{00B3}{\ensuremath{^3}}%                   ³
\DeclareUnicodeCharacter{2074}{\ensuremath{^4}}%                   ⁴
\DeclareUnicodeCharacter{2075}{\ensuremath{^5}}%                   ⁵
\DeclareUnicodeCharacter{2076}{\ensuremath{^6}}%                   ⁶
\DeclareUnicodeCharacter{2077}{\ensuremath{^7}}%                   ⁷
\DeclareUnicodeCharacter{2078}{\ensuremath{^8}}%                   ⁸
\DeclareUnicodeCharacter{2079}{\ensuremath{^9}}%                   ⁹
\DeclareUnicodeCharacter{207A}{\ensuremath{^+}}%                   ⁺
\DeclareUnicodeCharacter{207B}{\ensuremath{^-}}%                   ⁻
\DeclareUnicodeCharacter{207C}{\ensuremath{^=}}%                   ⁼
\DeclareUnicodeCharacter{207D}{\ensuremath{^(}}%                   ⁽
\DeclareUnicodeCharacter{207E}{\ensuremath{^)}}%                   ⁾
\DeclareUnicodeCharacter{207F}{\ensuremath{^n}}%                   ⁿ
\DeclareUnicodeCharacter{2071}{\ensuremath{^i}}%                   ⁱ
%s
% \DeclareUnicodeCharacter{2026}{\ensuremath{\dots}}%                …

% Generated from ~/.XCompose using:
% cat /tmp/cal.txt | cut -d '"' -f 2- | tr '"' ' ' | cut -d ' ' -f 1,6 \
% | while IFS=' ' read a b; do
%   echo -n "\\DeclareUnicodeCharacter{$(printf "%X" "'$a")}"
%   echo "{\\\\ensuremath{\\mathcal{$b}}}%         $a";
% done

\DeclareUnicodeCharacter{1D49C}{\ensuremath{\mathcal{A}}}%         𝒜
\DeclareUnicodeCharacter{212C}{\ensuremath{\mathcal{B}}}%          ℬ
\DeclareUnicodeCharacter{1D49E}{\ensuremath{\mathcal{C}}}%         𝒞
\DeclareUnicodeCharacter{1D49F}{\ensuremath{\mathcal{D}}}%         𝒟
\DeclareUnicodeCharacter{2130}{\ensuremath{\mathcal{E}}}%          ℰ
\DeclareUnicodeCharacter{2131}{\ensuremath{\mathcal{F}}}%          ℱ
\DeclareUnicodeCharacter{1D4A2}{\ensuremath{\mathcal{G}}}%         𝒢
\DeclareUnicodeCharacter{210B}{\ensuremath{\mathcal{H}}}%          ℋ
\DeclareUnicodeCharacter{2110}{\ensuremath{\mathcal{I}}}%          ℐ
\DeclareUnicodeCharacter{1D4A5}{\ensuremath{\mathcal{J}}}%         𝒥
\DeclareUnicodeCharacter{1D4A6}{\ensuremath{\mathcal{K}}}%         𝒦
\DeclareUnicodeCharacter{2112}{\ensuremath{\mathcal{L}}}%          ℒ
\DeclareUnicodeCharacter{2133}{\ensuremath{\mathcal{M}}}%          ℳ
\DeclareUnicodeCharacter{1D4A9}{\ensuremath{\mathcal{N}}}%         𝒩
\DeclareUnicodeCharacter{1D4AA}{\ensuremath{\mathcal{O}}}%         𝒪
\DeclareUnicodeCharacter{1D4AB}{\ensuremath{\mathcal{P}}}%         𝒫
\DeclareUnicodeCharacter{1D4AC}{\ensuremath{\mathcal{Q}}}%         𝒬
\DeclareUnicodeCharacter{211B}{\ensuremath{\mathcal{R}}}%          ℛ
\DeclareUnicodeCharacter{1D4AE}{\ensuremath{\mathcal{S}}}%         𝒮
\DeclareUnicodeCharacter{1D4AF}{\ensuremath{\mathcal{T}}}%         𝒯
\DeclareUnicodeCharacter{1D4B0}{\ensuremath{\mathcal{U}}}%         𝒰
\DeclareUnicodeCharacter{1D4B1}{\ensuremath{\mathcal{V}}}%         𝒱
\DeclareUnicodeCharacter{1D4B2}{\ensuremath{\mathcal{W}}}%         𝒲
\DeclareUnicodeCharacter{1D4B3}{\ensuremath{\mathcal{X}}}%         𝒳
\DeclareUnicodeCharacter{1D4B4}{\ensuremath{\mathcal{Y}}}%         𝒴
\DeclareUnicodeCharacter{1D4B5}{\ensuremath{\mathcal{Z}}}%         𝒵
\DeclareUnicodeCharacter{1D4B6}{\ensuremath{\mathcal{a}}}%         𝒶
\DeclareUnicodeCharacter{1D4B7}{\ensuremath{\mathcal{b}}}%         𝒷
\DeclareUnicodeCharacter{1D4B8}{\ensuremath{\mathcal{c}}}%         𝒸
\DeclareUnicodeCharacter{1D4B9}{\ensuremath{\mathcal{d}}}%         𝒹
\DeclareUnicodeCharacter{212F}{\ensuremath{\mathcal{e}}}%          ℯ
\DeclareUnicodeCharacter{1D4BB}{\ensuremath{\mathcal{f}}}%         𝒻
\DeclareUnicodeCharacter{210A}{\ensuremath{\mathcal{g}}}%          ℊ
\DeclareUnicodeCharacter{1D4BD}{\ensuremath{\mathcal{h}}}%         𝒽
\DeclareUnicodeCharacter{1D4BE}{\ensuremath{\mathcal{i}}}%         𝒾
\DeclareUnicodeCharacter{1D4BF}{\ensuremath{\mathcal{j}}}%         𝒿
\DeclareUnicodeCharacter{1D4C0}{\ensuremath{\mathcal{k}}}%         𝓀
\DeclareUnicodeCharacter{1D4C1}{\ensuremath{\mathcal{l}}}%         𝓁
\DeclareUnicodeCharacter{1D4C2}{\ensuremath{\mathcal{m}}}%         𝓂
\DeclareUnicodeCharacter{1D4C3}{\ensuremath{\mathcal{n}}}%         𝓃
\DeclareUnicodeCharacter{2134}{\ensuremath{\mathcal{o}}}%          ℴ
\DeclareUnicodeCharacter{1D4C5}{\ensuremath{\mathcal{p}}}%         𝓅
\DeclareUnicodeCharacter{1D4C6}{\ensuremath{\mathcal{q}}}%         𝓆
\DeclareUnicodeCharacter{1D4C7}{\ensuremath{\mathcal{r}}}%         𝓇
\DeclareUnicodeCharacter{1D4C8}{\ensuremath{\mathcal{s}}}%         𝓈
\DeclareUnicodeCharacter{1D4C9}{\ensuremath{\mathcal{t}}}%         𝓉
\DeclareUnicodeCharacter{1D4CA}{\ensuremath{\mathcal{u}}}%         𝓊
\DeclareUnicodeCharacter{1D4CB}{\ensuremath{\mathcal{v}}}%         𝓋
\DeclareUnicodeCharacter{1D4CC}{\ensuremath{\mathcal{w}}}%         𝓌
\DeclareUnicodeCharacter{1D4CD}{\ensuremath{\mathcal{x}}}%         𝓍
\DeclareUnicodeCharacter{1D4CE}{\ensuremath{\mathcal{y}}}%         𝓎
\DeclareUnicodeCharacter{1D4CF}{\ensuremath{\mathcal{z}}}%         𝓏
\makeatother
}>>>|)