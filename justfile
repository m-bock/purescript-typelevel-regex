test:
    node compile-test/run-tests.js

gen:
    yarn purs-to-md --input-purs demo/src/ReadmeDemo.purs --output-md README.md
    sed -i '/module ReadmeDemo where/,+1d' ./README.md