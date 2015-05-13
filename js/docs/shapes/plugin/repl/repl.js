/*jshint evil:true */
/*global Reveal:true, window:true, document:true, setTimeout:true,
  ace:true, TypeScript:true, console:true, Node:true, Text:true */

(function() {

  var loadFile = function(url) {
    var xhr;
    if(window.XMLHttpRequest) {
      xhr = new window.XMLHttpRequest();
    } else if(window.ActiveXObject) {
      xhr = new window.ActiveXObject('Microsoft.XMLHTTP');
    } else {
      return "";
    }
    xhr.open('GET', url, false);
    if(xhr.overrideMimeType) {
      xhr.overrideMimeType('text/plain');
    }
    xhr.send(null);
    if(xhr.status == 200) {
      return xhr.responseText;
    }
    return "";
  };

  var libDTS = loadFile("plugin/repl/lib.d.ts");

  var posForOffset = function(src, offset) {
    var row = 1, col = 0, pos = 0;
    for (; pos < offset; pos++) {
      if (src[pos] === "\n") {
        row++; col = 0;
      } else {
        col++;
      }
    }
    return { row: row, col: col };
  };

  var tsOutfile = function() {
    return { src: "", messages: [],
             Write: function(s) {
               this.src += s;
             },
             WriteLine: function(s) {
               this.src += s + "\n";
             },
             Close: function() {},
             log: function(s) {
               this.messages.push(s);
             }
           };
  };

  var tsErrorBlock = function(exprs, error) {
    var pos = error.start, i = 0, l = exprs.length, expr;
    for (; i < l; i++) {
      expr = exprs[i].ast;
      if (expr.minChar <= pos && expr.limChar >= pos) {
        return exprs[i];
      }
    }
    return null;
  };

  var tsDupeError = function(errors, error) {
    var i = 0, l = errors.length, current;
    for (; i < l; i++) {
      current = errors[i];
      if (current.msg === error.msg && current.start == error.start)
        return true;
    }
    return false;
  };

  var tsEmit = function(compiler, ast) {
    var outFile = tsOutfile(),
        emitter = new TypeScript.Emitter(compiler.typeChecker, "stdout",
                                         outFile, compiler.emitSettings,
                                         compiler.errorReporter);
    compiler.typeChecker.locationInfo = ast.locationInfo;
    emitter.emitJavascript(ast, TypeScript.TokenID.Comma, false);
    return outFile.src;
  };

  var tsCompile = window.tsCompile = function(src) {
    var out = [], errors = [], ast, i, l, gOut = tsOutfile();
    var compiler = new TypeScript.TypeScriptCompiler(gOut, gOut);

    compiler.parser.errorRecovery = true;
    compiler.setErrorCallback(function(start, len, msg, block) {
      var error = { msg: msg, block: block, start: start, len: len };
      if (!tsDupeError(errors, error))
        errors.push(error);
    });

    compiler.addUnit(libDTS, "lib.d.ts");
//    compiler.addUnit('var _ = function() {}', '');
    compiler.addUnit(src, "repl.ts");
//    compiler.typeCheck();

    ast = compiler.scripts.members[1].bod.members;
    out = ast.map(function(ast) {
      return {
        ast: ast,
        src: tsEmit(compiler, ast),
        errors: []
      };
    });

    for (i = 0, l = errors.length; i < l; i++) {
      var expr = tsErrorBlock(out, errors[i]);
      expr.errors.push(errors[i]);
    }

    return { exprs: out, errors: errors };
  };

  var describeFunction = function(fn) {
    var sig = fn.toString(), i = sig.indexOf("{");
    return sig.slice(0, i);
  };

  var describeAttrs = function(node) {
    var attrs = node.attributes, i = 0, l = attrs ? attrs.length : 0, o = "", attr;
    if (l === 0) return "";
    for (; i < l; i++) {
      attr = attrs.item(i);
      o += " " + attr.nodeName + "=\"" + attr.nodeValue + "\"";
    }
    return o;
  };

  var describeContent = function(node) {
    if (node.childNodes.length > 0 && node.childNodes.item(0) instanceof Text)
      return node.childNodes.item(0).nodeValue.trim();
    else return "...";
  };

  var describeNode = function(node) {
    var s = "<" + node.nodeName.toLowerCase() + describeAttrs(node);
    if (node.childNodes && node.childNodes.length)
      s += ">" + describeContent(node) + "</" + node.nodeName.toLowerCase() + ">";
    else if (node.nodeValue && node.nodeValue.length)
      s += ">" + node.nodeValue + "</" + node.nodeName.toLowerCase() + ">";
    else s += "/>";
    return s;
  };

  var describeValue = function(k, v) {
    if (typeof v === "function") return describeFunction(v);
    if (v instanceof Node) return describeNode(v);
    return v;
  };

  var stringify = function(obj) {
    var out = JSON.stringify(obj, describeValue);
    return (out.length > 60) ? JSON.stringify(obj, describeValue, 2) : out;
  };

  var commentify = function(prefix, msg) {
    return msg.split("\n").map(function(s) {
      return "//" + prefix + " " + s;
    }).join("\n");
  };

  var flashEditor = function(editor, type) {
    editor.container.classList.add("flash-" + type);
    setTimeout(function() {
      editor.container.classList.remove("flash-" + type);
    }, 50);
  };

  var evaluateExpAtPoint = function(editor) {
    var compiled, exprs, val, out = "", pos = 0, errors = false,
        context = editor.jsContext || "",
        offset = editor.jsContext ? editor.jsContext.length : 0,
        code = editor.getValue().replace(/^\/\/.*(\n|$)/gm, "");

    compiled = tsCompile(context + code);
    exprs = compiled.exprs;

    if (compiled.errors.length) {
      exprs.forEach(function(expr) {
        var end = expr.ast.limChar - offset;
        if (end < 0) {
          expr.errors.forEach(function(error) {
            var pos = posForOffset(context, error.start);
            console.error("In context: " + pos.row + ":" + pos.col +
                          ": " + error.msg);
          });
          return;
        }
        out += code.slice(pos, end);
        pos = end;
        if (expr.errors.length) {
          expr.errors.forEach(function(error) {
            out += "\n" + commentify("!!", error.msg);
          });
        }
      });
      editor.setValue(out, 1);
      pos = posForOffset(code, compiled.errors[0].start - offset);
      editor.gotoLine(pos.row, pos.col, true);
      flashEditor(editor, "error");
      return;
    }

    (function(__exprs) {
      var __i = 0, __l = __exprs.length;
      for (; __i < __l; __i++) {
        try {
          __exprs[__i].result = eval(__exprs[__i].src);
        } catch (e) {
          __exprs[__i].error = e.name + ": " + e.message;
        }
      }
    })(exprs);

    exprs.forEach(function(expr) {
      var end = expr.ast.limChar - offset;
      if (end < 0) return;
      while (code[end-1] === "\n" || code[end-1] === " ") end--;
      out += code.slice(pos, end);
      pos = end;
      if (expr.error !== undefined) {
        out += "\n//=> " + expr.error;
        errors = true;
      } else if (expr.result !== undefined) {
        val = stringify(expr.result);
        out += "\n" + commentify("=>", val);
      }
    });

    flashEditor(editor, errors ? "error" : "success");

    pos = editor.selection.getCursor();
    editor.setValue(out + "\n", 1);
    editor.selection.moveCursorToPosition(pos);
  };

  var createEditor = function(el) {
    var editor = ace.edit(el);
    editor.setTheme("ace/theme/dawn");
    editor.renderer.setShowGutter(false);
    editor.session.setMode("ace/mode/typescript");
    editor.session.setNewLineMode("unix");
    editor.session.setTabSize(2);
    editor.session.setUseSoftTabs(true);
    editor.session.setUseWrapMode(true);
    editor.session.setUseWorker(false);
    editor.setDisplayIndentGuides(false);

    editor.commands.addCommand({
      name: "evaluateExpAtPoint",
      bindKey: "Ctrl-S",
      exec: function() { evaluateExpAtPoint(editor); }
    });

    editor.commands.addCommand({
      name: "removeToLineEnd",
      bindKey: "Ctrl-K",
      exec: function() { editor.removeToLineEnd(); }
    });

    editor.commands.addCommand({
      name: "startOfLine",
      bindKey: "Ctrl-A",
      exec: function() { editor.selection.moveCursorLineStart(); }
    });

    editor.commands.addCommand({
      name: "endOfLine",
      bindKey: "Ctrl-E",
      exec: function() { editor.selection.moveCursorLineEnd(); }
    });

    editor.commands.addCommand({
      name: "forLoop",
      bindKey: "Ctrl-F",
      exec: function() { editor.insert("for (i = 0; i < list.length; i++) {}");
                  editor.selection.moveCursorLeft(); }
    });

    return editor;
  };

  var minIndent = function(text) {
    return text.split("\n").reduce(function(min, line) {
      if (line.trim().length === 0) return min;
      var indent = line.length - line.trimLeft().length;
      return min === null ? indent : Math.min(min, indent);
    }, null);
  };

  var alignIndents = function(text) {
    var indent = minIndent(text);
    return text.split("\n").map(function(line) {
      return line.slice(indent).trimRight();
    }).join("\n");
  };

  var cleanText = function(text) {
    text = alignIndents(text);
    while (text[0] === "\n") text = text.slice(1);
    while (text[text.length-1] === "\n") text = text.slice(0, text.length - 1);
    return text + "\n";
  };

  var installRepl = function(pre, context) {
    pre.classList.add("active");

    var repl = pre.repl = document.createElement("div");
    repl.classList.add("live-repl");
    repl.innerHTML = pre.innerHTML;
    pre.parentNode.appendChild(repl);
    window.editor = pre.editor = createEditor(repl);
    pre.editor.jsContext = context;
    pre.editor.focus();
  };

  var uninstallRepl = function(pre) {
    window.editor = null;
    pre.editor.destroy();
    pre.editor = null;
    pre.parentNode.removeChild(pre.repl);
    pre.repl = null;
    pre.classList.remove("active");
  };

  var findRepl = function(slide) {
    return slide.querySelector("pre.repl");
  };

  var findContext = function(slide) {
    var pre = slide.querySelector("pre.context");
    return pre ? pre.innerHTML : "";
  };

  var forEach = function(seq, fn) {
    var i = 0, l = seq.length;
    for (; i < l; i++) {
      fn(seq[i]);
    }
  };

  (function() {
    var css = document.createElement("link");
    css.setAttribute("rel", "stylesheet");
    css.setAttribute("href", "plugin/repl/repl.css");
    document.head.appendChild(css);

    forEach(document.querySelectorAll("pre.repl"), function(pre) {
      pre.innerHTML = cleanText(pre.innerHTML);
    });

    document.addEventListener("keydown", function(e) {
      if (e.altKey && e.keyCode == 33) {
        Reveal.navigatePrev();
        e.preventDefault();
      } else if (e.altKey && e.keyCode == 34) {
        Reveal.navigateNext();
        e.preventDefault();
      }
    }, false);

    var currentRepl = findRepl(Reveal.getCurrentSlide());
    if (currentRepl)
      installRepl(currentRepl, findContext(Reveal.getCurrentSlide()));

    var replTimer = null;

    Reveal.addEventListener("slidechanged", function(event) {
      var currentRepl = findRepl(event.currentSlide),
          previousRepl = findRepl(event.previousSlide);
      if (previousRepl) uninstallRepl(previousRepl);
      if (currentRepl) {
        if (replTimer !== null) window.clearTimeout(replTimer);
        replTimer = setTimeout(function() {
          replTimer = null;
          installRepl(currentRepl, findContext(Reveal.getCurrentSlide()));
        }, 1000);
      }
    });
  })();
})();
