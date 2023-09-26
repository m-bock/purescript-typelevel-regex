test:
    node compile-test/run-tests.js

gen:
    yarn purs-to-md --input-purs demo/src/ReadmeDemo.purs --output-md README.md