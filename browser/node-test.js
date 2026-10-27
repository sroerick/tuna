// node smoke for the jsoo bundle:
//   cat _build/default/browser/bin/main.bc.js browser/node-test.js > /tmp/tuna-test.js && node /tmp/tuna-test.js
// expect: identity -> normal 0; pair -> normal 200; self-apply under tiny fuel -> loop.
console.log(tunaEval("(lambda (x) x)", "", 10000, 10000));
console.log(tunaEval("(pair 0 0)", "", 10000, 10000));
console.log(tunaEval("(lambda (x) (x x))", "", 200, 10000));