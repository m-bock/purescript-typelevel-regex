import * as fs from "fs";
import * as cp from "child_process";
import dedent from "dedent";

const testFile = "test/Test/Tmp.purs";

const run = (src, expectedExitCode) => {
  fs.writeFileSync(testFile, dedent(src));
  const result = cp.spawnSync("spago", ["build"]);
  console.log(result.stderr.toString());
  if (result.status !== expectedExitCode) {
    process.exit(1);
  }
  fs.rmSync(testFile);
};

const shouldNotCompile = (src) => {
  run(src, 1);
};

const shouldCompile = (src) => {
  run(src, 0);
};

const mkModule = (src) => dedent`
  module Test.Tmp where
      
  import Regex as R

  ${src}
`;

const main = () => {
  shouldCompile(mkModule`
      str :: String
      str = R.match @"https?" @"http"
    `);

  shouldNotCompile(mkModule`
      str :: String
      str = R.match @"https?" @"ttp"
    `);
};

main();
