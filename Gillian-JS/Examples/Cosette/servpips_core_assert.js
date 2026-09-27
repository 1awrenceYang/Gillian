/* SERVPIPS WP1 core probe (E1): an interpreter error (a failed Cosette
   assertion) is streamed as end{error}; its final state is not kept. */
var x = symb_number();
var y = 0;
if (x > 0) { y = 1; }
Assert(y = 0);
