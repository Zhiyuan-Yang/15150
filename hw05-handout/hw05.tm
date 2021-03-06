<TeXmacs|1.0.7.21>

<style|generic>

<\body>
  <doc-data|<doc-title|hw05>|<doc-author|<\author-data|<author-name|zihan
  zhou>>
    \;
  </author-data>>>

  <section|>

  <section|>

  <subsection|>

  <\code>
    'a list * string =\<gtr\> string
  </code>

  <subsection|>

  this does not work\ 

  <\code>
    line 1: 'a tree*'b='c list
  </code>

  we know f return some list however in line 2 there is

  <\code>
    node ( f(L, n+1),...
  </code>

  we know node take only tree, list is definitely not tree

  <subsection|>

  <\code>
    int =\<gtr\> int
  </code>

  <subsection|>

  <subsubsection|>

  4, int. everything in let and in in the same scope. x is changed in line 7

  <subsubsection|>

  12.0 float, m is computed in line 8 ,becasue tmp is global and is 3.0 and x
  is 4.0

  <subsubsection|>

  2, int, because the change in line 7 is in a scope that has ended in line
  13

  <subsubsection|>

  float, 63.

  <subsection|>

  <subsubsection|>

  4,int , because\ 

  <\code>
    line 3: y= x+3
  </code>

  <subsubsection|>

  7,int because\ 

  <\code>
    line 5: y=7
  </code>

  <subsubsection|>

  5,int, because in line 4 y = 4 and\ 

  <\code>
    line 4 : g z = z + y\ 
  </code>

  <subsubsection|>

  6,int in line 6 line 7 , h n is (y-x)*n, y-x here is 6, so h x = 6

  <subsubsection|>

  11 because gx=5 hx =6

  \;

  <section|>

  <subsection|>

  <subsubsection|work>

  <\math>
    W<around*|(|0|)>=k<rsub|0>

    W<around*|(|d|)>=k<rsub|1>+W<around*|(|d-1|)>

    W<around*|(|d|)>\<in\>O<around*|(|d|)>
  </math>

  in the best case <math|d=log n>, so it is <math|O<around*|(|log n|)>>

  in the worst case <math|d=n>, so it is <math|O<around*|(|n|)>>

  <subsubsection|span>

  everything is sequential, so the span is exatly the same as the work.

  <subsection|>

  <subsection|>

  <\code>
    fun ILsort' ([])= empty\ 

    \ \ \| ILsort' (x::L)=Insert(x,ILsort'(L))

    fun ILsort t = ILsort'(inorder(t))
  </code>

  <\lemma>
    \;

    For all x: int and t: int tree, If sorted (inorder t) <math|\<cong\>>
    true,\ 

    then sorted (inorder (Insert(x, t)))<math|\<cong\>>= true.
  </lemma>

  <\proposition>
    \;

    For all L: int list,<next-line>sorted (inorder (ILsort' L))
    <math|\<cong\>> true
  </proposition>

  <\proof>
    by tree list induction on L

    two cases:

    L= nil \ or L = x::L'

    Base case: L= nil:

    sorted (inorder (ILsort' nil))<math|\<cong\>>sorted(inorder(empty))<math|\<cong\>sorted<around*|(|nil|)>\<cong\>true>

    Inductive case:

    To show:\ 

    sorted (inorder (ILsort' x::L')) <math|\<cong\>> true

    IH:

    sorted (inorder (ILsort' L')) <math|\<cong\>> true

    Proof:

    <math|<tabular|<tformat|<table|<row|<cell|sorted<around*|(|inorder<around*|(|ILsort<rprime|'>
    x\<colons\>L<rprime|'>|)>|)>>|<cell|\<cong\>>|<cell|sorted<around*|(|inorder<around*|(|Insert(x,ILsort'(L))|)>|)>>|<cell|by
    def of ILsort<rprime|'>>>|<row|<cell|>|<cell|\<cong\>>|<cell|true>|<cell|by
    Lemma 2 and IH>>>>>>
  </proof>

  <\proposition>
    \;

    For all t: int tree,<next-line>sorted (inorder (ILsort t))
    <math|\<cong\>> true
  </proposition>

  <\proof>
    \;

    sorted (inorder (ILsort t))=sorted (inorder (ILsort' inorder(t) ))=true
  </proof>

  <subsection|>

  everything is sequential work is the same as the span, i will only do work.

  <\math>
    W<rsub|S><around*|(|n|)>=W<rsub|S<rprime|'>><around*|(|n|)>+k<rsub|0>

    W<rsub|S<rprime|'>><around*|(|0|)>=k<rsub|1>

    W<rsub|S<rprime|'>><around*|(|n|)>=W<rsub|S><around*|(|n-1|)>+W<rsub|Insert><around*|(|n-1|)>+k<rsub|2>

    In worst case:\ 

    W<rsub|S<rprime|'>><around*|(|n|)>=W<rsub|S><around*|(|n-1|)>+k<rsub|3><around*|(|n-1|)>+k<rsub|2>

    W<rsub|S>\<in\>W<rsub|S<rprime|'>>\<in\>O<around*|(|n<rsup|2>|)>

    In best case:

    W<rsub|S<rprime|'>><around*|(|n|)>=W<rsub|S><around*|(|n-1|)>+k<rsub|3><around*|(|log
    <around*|(|n-1|)>|)>+k<rsub|2>

    W<rsub|S>\<in\>W<rsub|S<rprime|'>>\<in\>O<around*|(|n<rsup|>*log n|)>
  </math>

  <subsection|>

  always rebalance the tree after each insert

  <subsection|>

  Work is the same, Span is worse than merge. Because Insertion sort does not
  allow parallel computation.

  <section|>

  \;

  \ 

  \;

  <\code>
    \;
  </code>
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-10|<tuple|2.4.4|?>>
    <associate|auto-11|<tuple|2.5|?>>
    <associate|auto-12|<tuple|2.5.1|?>>
    <associate|auto-13|<tuple|2.5.2|?>>
    <associate|auto-14|<tuple|2.5.3|?>>
    <associate|auto-15|<tuple|2.5.4|?>>
    <associate|auto-16|<tuple|2.5.5|?>>
    <associate|auto-17|<tuple|3|?>>
    <associate|auto-18|<tuple|3.1|?>>
    <associate|auto-19|<tuple|3.1.1|?>>
    <associate|auto-2|<tuple|2|?>>
    <associate|auto-20|<tuple|3.1.2|?>>
    <associate|auto-21|<tuple|3.2|?>>
    <associate|auto-22|<tuple|3.3|?>>
    <associate|auto-23|<tuple|3.4|?>>
    <associate|auto-24|<tuple|3.5|?>>
    <associate|auto-25|<tuple|3.6|?>>
    <associate|auto-26|<tuple|4|?>>
    <associate|auto-3|<tuple|2.1|?>>
    <associate|auto-4|<tuple|2.2|?>>
    <associate|auto-5|<tuple|2.3|?>>
    <associate|auto-6|<tuple|2.4|?>>
    <associate|auto-7|<tuple|2.4.1|?>>
    <associate|auto-8|<tuple|2.4.2|?>>
    <associate|auto-9|<tuple|2.4.3|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <with|par-left|<quote|1tab>|2.1<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3>>

      <with|par-left|<quote|1tab>|2.2<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4>>

      <with|par-left|<quote|1tab>|2.3<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5>>

      <with|par-left|<quote|1tab>|2.4<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6>>

      <with|par-left|<quote|2tab>|2.4.1<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-7>>

      <with|par-left|<quote|2tab>|2.4.2<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-8>>

      <with|par-left|<quote|2tab>|2.4.3<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-9>>

      <with|par-left|<quote|2tab>|2.4.4<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-10>>

      <with|par-left|<quote|1tab>|2.5<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-11>>

      <with|par-left|<quote|2tab>|2.5.1<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-12>>

      <with|par-left|<quote|2tab>|2.5.2<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-13>>

      <with|par-left|<quote|2tab>|2.5.3<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-14>>

      <with|par-left|<quote|2tab>|2.5.4<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-15>>

      <with|par-left|<quote|2tab>|2.5.5<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-16>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-17><vspace|0.5fn>

      <with|par-left|<quote|1tab>|3.1<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-18>>

      <with|par-left|<quote|2tab>|3.1.1<space|2spc>work
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-19>>

      <with|par-left|<quote|2tab>|3.1.2<space|2spc>span
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-20>>

      <with|par-left|<quote|1tab>|3.2<space|2spc>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-21>>
    </associate>
  </collection>
</auxiliary>