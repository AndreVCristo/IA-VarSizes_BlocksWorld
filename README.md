<h1>Exemplo de execução do código:</h1>

<p><code>state1(S1)</code>,<code>goal1(G1)</code>,<code>plan(S1,G1,Plan,_)</code></p>

<ul>
  <li><code>state1(S1)</code> define o estado inicial do mundo, por exemplo, onde estão os blocos, quais locais estão limpos, e quaisquer outras condições relevantes.</li>
  <li><code>goal1(G1)</code> define os objetivos que o sistema precisa alcançar, como mover blocos para certas posições, ou limpar locais específicos.</li>
  <li><code>plan(S1, G1, Plan, _)</code> é responsável por gerar o plano.</li>
</ul>

<h2>Definição de Estado Inicial e Meta</h2>

<p><strong>Estado Inicial:</strong> O estado inicial define a disposição atual dos blocos e posições no ambiente. Por exemplo:
  <code> state1([occupied((1,1)),
         clear((1,2)),
         clear((1,3)),
         clear((1,4)),
         occupied((2,1)),
         clear((2,2)),
         clear((2,3)),
         clear((2,4)),
         clear((3,1)),
         clear((3,2)),
         clear((3,3)),
         clear((3,4)),
         occupied((4,1)),
         occupied((4,2)),
         clear((4,3)),
         clear((4,4)),
         clear((5,1)),
         occupied((5,2)),
         clear((5,3)),
         clear((5,4)),
         occupied((6,1)),
         occupied((6,2)),
         clear((6,3)),
         clear((6,4)),
         occupied((1,0)),
         occupied((2,0)),
         occupied((3,0)),
         occupied((4,0)),
         occupied((5,0)),
         occupied((6,0)),
         clear((1,5)),
         clear((2,5)),
         clear((3,5)),
         clear((4,5)),
         clear((5,5)),
         clear((6,5)),
         on(a,(4,1)),
         on(b,(6,1)),
         on(c,(1,1)),
         on(d,(4,2))
       ]). </code> </p>

<p><strong>Meta:</strong> A meta define as condições que o sistema precisa alcançar. Exemplo:
<code>
  goal1([on(a,(1,2)),
        on(b,(6,1)),
        on(c,(1,1)),
        on(d,(3,1))
      ]). 
</code></p>
