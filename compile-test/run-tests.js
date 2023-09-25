import * as fs from "fs";
import * as cp from "child_process";
import dedent from "dedent";
import { shouldCompile, shouldNotCompile } from "./util.js";
import { assert } from "console";
import { throws } from "assert";

const mkModule = (src) => dedent`
  module Test.Tmp where
      
  import Type.Regex as R

  ${src}
`;

const testRegexOk = (regex, str) => {
  shouldCompile(mkModule(`
      str :: String
      str = R.guard @"${regex}" @"${str}"
    `));

  const jsRegex = new RegExp(regex);

  assert(jsRegex.test(str), "should have matched");
};

const testRegexFail = (regex, str, err) => {
  shouldNotCompile(mkModule(`
      str :: String
      str = R.guard @"${regex}" @"${str}"
    `), err);

  throws(() => {
    const jsRegex = new RegExp(regex);
    if (!jsRegex.test(str)) {
      throw new Error("");
    }
  }, `should have thrown: ${regex} matches ${str}`);
};

const main = () => {
  testRegexOk("hello", "hello");
  testRegexFail("hello", "hi", "Regex failed to match");

  testRegexOk("foo|bar|baz", "foo");
  testRegexOk("foo|bar|baz", "bar");
  testRegexOk("foo|bar|baz", "baz");
  testRegexFail("foo|bar|baz", "qua", "Regex failed to match");

  testRegexOk("a*", "");
  testRegexOk("a*", "a");
  testRegexOk("a*", "aa");
  testRegexOk("a*", "aaa");
  //testRegexFail("a*", "b", "Regex failed to match");

  // testRegexOk("(foo)*", "");
  // testRegexOk("(foo)*", "foo");
  // testRegexOk("(foo)*", "foofoo");
  // testRegexFail("(foo)*", "qua", "Regex failed to match");

  // testRegexOk("(foo|bar)*", "");
  // testRegexOk("(foo|bar)*", "foo");
  // testRegexOk("(foo|bar)*", "foobar");
  // testRegexOk("(foo|bar)*", "foobarfoo");

};

main();
