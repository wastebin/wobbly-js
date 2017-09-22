function check(thing, stuff) {
  return stuff.map((s, i) => s == thing[i]).reduce((a, b) => a && b, true);
}
function expect(stream, thing, stuff) {
  if (!check(thing, stuff)) stream.error(`Expected ${stuff.join(" ")}, found ${thing.join(" ")}`);
}
function trySkip(read, thing, stuff) {
  if (check(thing, stuff)) {
    read();
    return true;
  }
  return false;
}
function skip(stream, read, thing, stuff) {
  expect(stream, thing, stuff);
  const old = thing;
  read();
  return old;
}

function LEB(n) {
  const bytes = [];

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
function uLEB(n) {
  const bytes = [];

  do {
    let byte = n & 0x7F;
    n >>= 7;
    if (n) byte |= 0x80;
    bytes.push(byte);
  } while (n);

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

  function parseInst() {
    if (trySkip(t, token, ["word", "get_local"])) {
      return ["get_local", skip(t, read, token, ["num"])];
    }
    else if (trySkip(t, token, ["word", "i32.add"])) return ["add", "i32"];
    else t.error(`Unknown instruction ${token.join(" ")}`);
  }

  function parseSec() {
    if (trySkip(t, token, ["word", "type"])) {
      skip(t, read, token, ["word", "fn"]);
      skip(t, read, token, ["punc", "("]);
      const argTypes = [];
      while (check(token, ["word"])) {
        if (token[1] in types) argTypes.push(token[1]);
        else t.error(`Expected a type, found ${token.join(" ")}`);
        skip(t, read, token, ["word"]);
      }
      skip(t, read, token, ["punc", ")"]);
      skip(t, read, token, ["punc", "("]);
      const retTypes = [];
      while (check(token, ["word"])) {
        if (token[1] in types) retTypes.push(token[1]);
        else t.error(`Expected a type, found ${token.join(" ")}`);
        skip(t, read, token, ["word"]);
      }
      skip(t, read, token, ["punc", ")"]);

      return ["type", argTypes, retTypes];
    }
    else if (trySkip(t, token, ["word", "fn"])) {
      return ["fn", skip(t, read, token, ["num"])];
    }
    else if (trySkip(t, token, ["word", "export"])) {
      const handle = skip(t, read, token, ["str"]);
      skip(t, read, token, ["word", "fn"]);
      return ["export", handle, skip(t, read, token, ["num"])];
    }
    else if (trySkip(t, token, ["word", "body"])) {
      const numLocals = skip(t, read, token, ["num"])[1];
      const body = [];
      while (!done && !trySkip(t, token, ["word", "end"])) {
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
