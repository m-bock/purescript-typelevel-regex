import * as fs from "fs";
import * as cp from "child_process";
import dedent from "dedent";
import { shouldCompile, shouldNotCompile } from "./util.js";

const mkModule = (src) => dedent`
  module Test.Tmp where
      
  import Type.Regex as R

  ${src}
`;

const main = () => {
  shouldCompile(mkModule`
      str :: String
      str = R.guard @"hello" @"hello"
    `);

  shouldNotCompile(
    mkModule`
      str :: String
      str = R.guard @"hello" @"hi"
    `,
    "Regex failed to match"
  );
};

main();
