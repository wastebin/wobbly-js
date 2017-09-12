function leb(n) {
  const bytes = [], size = 32;

  let more = true;

  while (more) {
    let byte = n & 0x7F;
    n >>= 7;

    if (n === 0 && ~byte & 0x40 || n == -1 && byte & 0x40) more = 0;
    else byte |= 0x80;

    bytes.push(byte);
  }
  return bytes;
}

function stream(src) {
  let pos = 0, line = 1, col = 0;
  return {gen: gen(), error};

  function *gen() {
    while (true) {
      const c = src[pos++];
      if (c == '\n') line++, col = 0;
      else col++;
      if (pos >= src.length) return c;
      yield c;
    }
  }
  function error(msg) {
    throw new Error(`${msg} @ ${line}:${col}`);
  }
}

function tokenStream(src) {
  const s = stream(src);
  let done, c;
  function read() {
    ({done, value: c} = s.gen.next());
    return c;
  }
  read();

  return {gen: gen(), error: s.error};

  function isWS(c) {
    return /\s/.test(c);
  }
  function isDigit(c) {
    return /[0-9.-]/.test(c);
  }
  function isWordStart(c) {
    return /[a-z]/.test(c);
  }
  function isWord(c) {
    return /[a-z0-9-_.]/.test(c);
  }
  function isPunc(c) {
    return ~",:()[]{}".indexOf(c);
  }
  function readWhile(p) {
    let str = "";
    while (!done) {
      if (!p(c)) break;
      str += c;
      read();
    }
    return str;
  }
  function readEscaped(end) {
    let esc = false, str = "";
    while (!done) {
      read();
      if (esc) {
        str += c;
        esc = false;
      }
      else if (c == '\\') esc = true;
      else if (c == end) break;
      else str += c;
    }
    read();
    return str;
  }
  function *gen() {
    while (readWhile(isWS), !done) {
      let token;
      if (c == ';') readWhile(c => c != '\n');
      else if (c == '"') token = ["str", readEscaped('"')];
      else if (isDigit(c)) token = ["num", parseFloat(readWhile(isDigit))];
      else if (isWordStart(c)) token = ["word", readWhile(isWord)];
      else if (isPunc(c)) token = ["punc", c], read();
      readWhile(isWS);
      if (done) return token;
      yield token;
    }
  }
}

function parse(src) {
  const kinds = {
    fn: 0,
    table: 1,
    mem: 2,
    global: 3
  };
  const types = {
    i32: leb(-0x01),
    i64: leb(-0x02),
    f32: leb(-0x03),
    f64: leb(-0x04),
    "any-func": leb(-0x10),
    func: leb(-0x20),
    block: leb(0x40)
  };

  const t = tokenStream(src);
  let done, token;
  function read() {
    ({done, value: token} = t.gen.next());
    return token;
  }
  read();

  function check(stuff) {
    return stuff.map((s, i) => s == token[i]).reduce((a, b) => a && b, true);
  }
  function expect(stuff) {
    if (!check(stuff)) t.error(`Expected ${stuff.join(" ")}, found ${token.join(" ")}`);
  }
  function trySkip(stuff) {
    if (check(stuff)) {
      read();
      return true;
    }
    return false;
  }
  function skip(stuff) {
    expect(stuff);
    const old = token;
    read();
    return old;
  }

  function parseInst() {
    if (trySkip(["word", "get_local"])) {
      return ["get_local", skip(["num"])];
    }
    else if (trySkip(["word", "i32.add"])) return ["add", "i32"];
    else t.error(`Unknown instruction ${token.join(" ")}`);
  }

  function parseSec() {
    if (trySkip(["word", "type"])) {
      skip(["word", "fn"]);
      skip(["punc", "("]);
      const argTypes = [];
      while (check(["word"])) {
        if (token[1] in types) argTypes.push(token[1]);
        else t.error(`Expected a type, found ${token.join(" ")}`);
        skip(["word"]);
      }
      skip(["punc", ")"]);
      skip(["punc", "("]);
      const retTypes = [];
      while (check(["word"])) {
        if (token[1] in types) retTypes.push(token[1]);
        else t.error(`Expected a type, found ${token.join(" ")}`);
        skip(["word"]);
      }
      skip(["punc", ")"]);

      return ["type", argTypes, retTypes];
    }
    else if (trySkip(["word", "fn"])) {
      return ["fn", skip(["num"])];
    }
    else if (trySkip(["word", "export"])) {
      const handle = skip(["str"]);
      skip(["word", "fn"]);
      return ["export", handle, skip(["num"])];
    }
    else if (trySkip(["word", "body"])) {
      const numLocals = skip(["num"])[1];
      const body = [];
      while (!done && !trySkip(["word", "end"])) {
        body.push(parseInst());
      }
      return ["body", numLocals, body];
    }
  }

  const sections = [];
  while (!done) {
    sections.push(parseSec());
  }
  return ["module", sections];
}

function compile(src) {
  function check(issue, stuff) {
    return stuff.map((s, i) => s == issue[i]).reduce((a, b) => a && b, true);
  }

  const ast = parse(src);
  const main = ast[1];

  let types = [],
      funcs = [],
      exports = [],
      code = [];

  for (let i = 0; i < main.length; i++) {
    const section = main[i];
    if (check(section, ["type"])) {
      types.push(section);
    }
    else if (check(section, ["fn"])) {
      funcs.push(section);
    }
    else if (check(section, ["export"])) {
      funcs.push(section);
    }
    else if (check(section, ["body"])) {
      code.push(section);
    }
  }
}
