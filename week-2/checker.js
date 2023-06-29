const fs = require("fs");

const contents = fs.readFileSync("error.log", { encoding: "utf-8" });

const cLines = contents.split("\n").slice(0, -1);

const getTimestamp = (x) => {
  const xs = x.split(" ");
  if (xs[0] == "I" || xs[0] == "W") {
    return parseInt(xs[1]);
  } else {
    return parseInt(xs[2]);
  }
}

const getErrorCode = (x) => {
  const xS = x.split(" ");
  if (xS[0] == "E") {
    return parseInt(xS[1]);
  }
  return 0;
}

const final = cLines.sort((a, b) => {
  return (getTimestamp(a) - getTimestamp(b))
}).filter(a => getErrorCode(a) >= 50)

console.log(final);
