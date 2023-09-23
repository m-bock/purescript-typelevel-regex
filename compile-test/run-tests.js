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
  }, "should have thrown");
};

const main = () => {
  testRegexOk("hello", "hello");
  testRegexFail("hello", "hi", "Regex failed to match");
};

main();
