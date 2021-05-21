"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function (o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    Object.defineProperty(o, k2, { enumerable: true, get: function () { return m[k]; } });
}) : (function (o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function (o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function (o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function () { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function () { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
exports.getValue = (editor) => () => {
    return editor.getValue();
};
exports.colorizeImpl = function (text) {
    return function (languageId) {
        return function (options) {
            return function (editor) {
                return editor.colorize(text, languageId, options);
            };
        };
    };
};
exports.setValue = (value) => (editor) => () => {
    console.log("Value len", value.length)
    editor.setValue(value)
    console.log("Value len after", editor.getValue().length)
};
exports.layout = (editor) => () => editor.layout()
exports.defineThemeImpl = function (monacoInstance) {
    return function (name) {
        return function (theme) {
            return function () {
                return monacoInstance.editor.defineTheme(name, theme);
            };
        };
    };
};
exports.setThemeImpl = function (monacoInstance) {
    return function (theme) {
        return function () {
            return monacoInstance.editor.setTheme(theme);
        };
    };
};
exports.registerLanguageImpl = function (monacoInstance) {
    return function (languageId) {
        return function () {
            monacoInstance.languages.register({ id: languageId });
        };
    };
};
exports.setMonarchTokensProviderImpl = function (monacoInstance) {
    return function (name) {
        return function (monarchLanguage) {
            return function () {
                return monacoInstance.languages.setMonarchTokensProvider(name, monarchLanguage);
            };
        };
    };
};
exports.horizonTheme = function (bg) {
    return ({
        base: "vs",
        inherit: true,
        rules: [
            {
                foreground: "#33333380",
                fontStyle: "italic",
                token: "comment"
            },
            {
                foreground: "#DC3318",
                token: "constant"
            },
            {
                foreground: "#F77D26",
                token: "entity.name"
            },
            {
                foreground: "#1D8991",
                token: "entity.name.function"
            },
            {
                foreground: "#DA103F",
                token: "entity.name.tag"
            },
            {
                foreground: "#F77D26",
                token: "entity.name.type"
            },
            {
                foreground: "#DA103F",
                token: "variable"
            },
            {
                foreground: "#DA103F",
                token: "entity.name.variable"
            },
            {
                foreground: "#8A31B9",
                token: "keyword"
            },
            {
                foreground: "#333333",
                token: "keyword.operator"
            },
            {
                foreground: "#8A31B9",
                token: "keyword.operator.new"
            },
            {
                foreground: "#8A31B9",
                token: "keyword.operator.expression"
            },
            {
                foreground: "#8A31B9",
                token: "keyword.operator.logical, keyword.operator.delete"
            },
            {
                foreground: "#8A31B9",
                token: "keyword.operator.delete"
            },
            {
                foreground: "#DC3318",
                token: "keyword.other.unit"
            },
            {
                foreground: "#F6661EB3",
                fontStyle: "italic",
                token: "markup.quote"
            },
            {
                foreground: "#DA103F",
                token: "entity.name.section"
            },
            {
                foreground: "#8A31B9",
                token: "storage"
            },
            {
                foreground: "#F6661E",
                token: "string"
            },
            {
                foreground: "#F77D26",
                token: "support"
            },
            {
                foreground: "#1D8991",
                token: "support.function"
            },
            {
                foreground: "#DA103F",
                token: "support.variable"
            },
            {
                foreground: "#DA103F",
                token: "support.type.property-name"
            },
            {
                foreground: "#DA103F",
                token: "meta.object-literal.key"
            },
            {
                foreground: "#333333",
                token: "support.type.property-name.css"
            },
            {
                foreground: "#F77D26",
                fontStyle: "italic",
                token: "variable.language"
            },
            {
                fontStyle: "italic",
                token: "variable.parameter"
            },
            {
                foreground: "#DA103FB3",
                token: "punctuation.definition.tag"
            },
            {
                foreground: "#333333",
                token: "punctuation.separator"
            },
        ],
        colors: {
            "editor.foreground": "#000000",
            "editor.background": bg, //"#FDF0ED" 
            "editor.selectionBackground": "#F9CBBE80",
            "editor.lineHighlightBackground": "#B5D5FF",
            "editor.lineHighlightBorder": "#B5D5FF",
            "editorCursor.foreground": "#333333",
            "editorWhitespace.foreground": "#BFBFBF",
        },
    });
};
exports.nightOwlTheme = function (bg) {
    return ({
        base: "vs-dark",
        inherit: true,
        rules: [
            {
                foreground: "637777",
                fontStyle: "italic",
                token: "comment",
            },
            {
                foreground: "addb67",
                token: "string",
            },
            {
                foreground: "ecc48d",
                token: "vstring.quoted",
            },
            {
                foreground: "ecc48d",
                token: "variable.other.readwrite.js",
            },
            {
                foreground: "5ca7e4",
                token: "string.regexp",
            },
            {
                foreground: "5ca7e4",
                token: "string.regexp keyword.other",
            },
            {
                foreground: "5f7e97",
                token: "meta.function punctuation.separator.comma",
            },
            {
                foreground: "f78c6c",
                token: "constant.numeric",
            },
            {
                foreground: "f78c6c",
                token: "constant.character.numeric",
            },
            {
                foreground: "addb67",
                token: "variable",
            },
            {
                foreground: "c792ea",
                token: "keyword",
            },
            {
                foreground: "c792ea",
                token: "punctuation.accessor",
            },
            {
                foreground: "c792ea",
                token: "storage",
            },
            {
                foreground: "c792ea",
                token: "meta.var.expr",
            },
            {
                foreground: "c792ea",
                token: "meta.class meta.method.declaration meta.var.expr storage.type.jsm",
            },
            {
                foreground: "c792ea",
                token: "storage.type.property.js",
            },
            {
                foreground: "c792ea",
                token: "storage.type.property.ts",
            },
            {
                foreground: "c792ea",
                token: "storage.type.property.tsx",
            },
            {
                foreground: "82aaff",
                token: "storage.type",
            },
            {
                foreground: "ffcb8b",
                token: "entity.name.class",
            },
            {
                foreground: "ffcb8b",
                token: "meta.class entity.name.type.class",
            },
            {
                foreground: "addb67",
                token: "entity.other.inherited-class",
            },
            {
                foreground: "82aaff",
                token: "entity.name.function",
            },
            {
                foreground: "addb67",
                token: "punctuation.definition.variable",
            },
            {
                foreground: "d3423e",
                token: "punctuation.section.embedded",
            },
            {
                foreground: "d6deeb",
                token: "punctuation.terminator.expression",
            },
            {
                foreground: "d6deeb",
                token: "punctuation.definition.arguments",
            },
            {
                foreground: "d6deeb",
                token: "punctuation.definition.array",
            },
            {
                foreground: "d6deeb",
                token: "punctuation.section.array",
            },
            {
                foreground: "d6deeb",
                token: "meta.array",
            },
            {
                foreground: "d9f5dd",
                token: "punctuation.definition.list.begin",
            },
            {
                foreground: "d9f5dd",
                token: "punctuation.definition.list.end",
            },
            {
                foreground: "d9f5dd",
                token: "punctuation.separator.arguments",
            },
            {
                foreground: "d9f5dd",
                token: "punctuation.definition.list",
            },
            {
                foreground: "d3423e",
                token: "string.template meta.template.expression",
            },
            {
                foreground: "d6deeb",
                token: "string.template punctuation.definition.string",
            },
            {
                foreground: "c792ea",
                fontStyle: "italic",
                token: "italic",
            },
            {
                foreground: "addb67",
                fontStyle: "bold",
                token: "bold",
            },
            {
                foreground: "82aaff",
                token: "constant.language",
            },
            {
                foreground: "82aaff",
                token: "punctuation.definition.constant",
            },
            {
                foreground: "82aaff",
                token: "variable.other.constant",
            },
            {
                foreground: "7fdbca",
                token: "support.function.construct",
            },
            {
                foreground: "7fdbca",
                token: "keyword.other.new",
            },
            {
                foreground: "82aaff",
                token: "constant.character",
            },
            {
                foreground: "82aaff",
                token: "constant.other",
            },
            {
                foreground: "f78c6c",
                token: "constant.character.escape",
            },
            {
                foreground: "addb67",
                token: "entity.other.inherited-class",
            },
            {
                foreground: "d7dbe0",
                token: "variable.parameter",
            },
            {
                foreground: "7fdbca",
                token: "entity.name.tag",
            },
            {
                foreground: "cc2996",
                token: "punctuation.definition.tag.html",
            },
            {
                foreground: "cc2996",
                token: "punctuation.definition.tag.begin",
            },
            {
                foreground: "cc2996",
                token: "punctuation.definition.tag.end",
            },
            {
                foreground: "addb67",
                token: "entity.other.attribute-name",
            },
            {
                foreground: "addb67",
                token: "entity.name.tag.custom",
            },
            {
                foreground: "82aaff",
                token: "support.function",
            },
            {
                foreground: "82aaff",
                token: "support.constant",
            },
            {
                foreground: "7fdbca",
                token: "upport.constant.meta.property-value",
            },
            {
                foreground: "addb67",
                token: "support.type",
            },
            {
                foreground: "addb67",
                token: "support.class",
            },
            {
                foreground: "addb67",
                token: "support.variable.dom",
            },
            {
                foreground: "7fdbca",
                token: "support.constant",
            },
            {
                foreground: "7fdbca",
                token: "keyword.other.special-method",
            },
            {
                foreground: "7fdbca",
                token: "keyword.other.new",
            },
            {
                foreground: "7fdbca",
                token: "keyword.other.debugger",
            },
            {
                foreground: "7fdbca",
                token: "keyword.control",
            },
            {
                foreground: "c792ea",
                token: "keyword.operator.comparison",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.flow.js",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.flow.ts",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.flow.tsx",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.ruby",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.module.ruby",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.class.ruby",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.def.ruby",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.loop.js",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.loop.ts",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.import.js",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.import.ts",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.import.tsx",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.from.js",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.from.ts",
            },
            {
                foreground: "c792ea",
                token: "keyword.control.from.tsx",
            },
            {
                foreground: "ffffff",
                background: "ff2c83",
                token: "invalid",
            },
            {
                foreground: "ffffff",
                background: "d3423e",
                token: "invalid.deprecated",
            },
            {
                foreground: "7fdbca",
                token: "keyword.operator",
            },
            {
                foreground: "c792ea",
                token: "keyword.operator.relational",
            },
            {
                foreground: "c792ea",
                token: "keyword.operator.assignement",
            },
            {
                foreground: "c792ea",
                token: "keyword.operator.arithmetic",
            },
            {
                foreground: "c792ea",
                token: "keyword.operator.bitwise",
            },
            {
                foreground: "c792ea",
                token: "keyword.operator.increment",
            },
            {
                foreground: "c792ea",
                token: "keyword.operator.ternary",
            },
            {
                foreground: "637777",
                token: "comment.line.double-slash",
            },
            {
                foreground: "cdebf7",
                token: "object",
            },
            {
                foreground: "ff5874",
                token: "constant.language.null",
            },
            {
                foreground: "d6deeb",
                token: "meta.brace",
            },
            {
                foreground: "c792ea",
                token: "meta.delimiter.period",
            },
            {
                foreground: "d9f5dd",
                token: "punctuation.definition.string",
            },
            {
                foreground: "ff5874",
                token: "constant.language.boolean",
            },
            {
                foreground: "ffffff",
                token: "object.comma",
            },
            {
                foreground: "7fdbca",
                token: "variable.parameter.function",
            },
            {
                foreground: "80cbc4",
                token: "support.type.vendor.property-name",
            },
            {
                foreground: "80cbc4",
                token: "support.constant.vendor.property-value",
            },
            {
                foreground: "80cbc4",
                token: "support.type.property-name",
            },
            {
                foreground: "80cbc4",
                token: "meta.property-list entity.name.tag",
            },
            {
                foreground: "57eaf1",
                token: "meta.property-list entity.name.tag.reference",
            },
            {
                foreground: "f78c6c",
                token: "constant.other.color.rgb-value punctuation.definition.constant",
            },
            {
                foreground: "ffeb95",
                token: "constant.other.color",
            },
            {
                foreground: "ffeb95",
                token: "keyword.other.unit",
            },
            {
                foreground: "c792ea",
                token: "meta.selector",
            },
            {
                foreground: "fad430",
                token: "entity.other.attribute-name.id",
            },
            {
                foreground: "80cbc4",
                token: "meta.property-name",
            },
            {
                foreground: "c792ea",
                token: "entity.name.tag.doctype",
            },
            {
                foreground: "c792ea",
                token: "meta.tag.sgml.doctype",
            },
            {
                foreground: "d9f5dd",
                token: "punctuation.definition.parameters",
            },
            {
                foreground: "ecc48d",
                token: "string.quoted",
            },
            {
                foreground: "ecc48d",
                token: "string.quoted.double",
            },
            {
                foreground: "ecc48d",
                token: "string.quoted.single",
            },
            {
                foreground: "addb67",
                token: "support.constant.math",
            },
            {
                foreground: "addb67",
                token: "support.type.property-name.json",
            },
            {
                foreground: "addb67",
                token: "support.constant.json",
            },
            {
                foreground: "c789d6",
                token: "meta.structure.dictionary.value.json string.quoted.double",
            },
            {
                foreground: "80cbc4",
                token: "string.quoted.double.json punctuation.definition.string.json",
            },
            {
                foreground: "ff5874",
                token: "meta.structure.dictionary.json meta.structure.dictionary.value constant.language",
            },
            {
                foreground: "d6deeb",
                token: "variable.other.ruby",
            },
            {
                foreground: "ecc48d",
                token: "entity.name.type.class.ruby",
            },
            {
                foreground: "ecc48d",
                token: "keyword.control.class.ruby",
            },
            {
                foreground: "ecc48d",
                token: "meta.class.ruby",
            },
            {
                foreground: "7fdbca",
                token: "constant.language.symbol.hashkey.ruby",
            },
            {
                foreground: "e0eddd",
                background: "a57706",
                fontStyle: "italic",
                token: "meta.diff",
            },
            {
                foreground: "e0eddd",
                background: "a57706",
                fontStyle: "italic",
                token: "meta.diff.header",
            },
            {
                foreground: "ef535090",
                fontStyle: "italic",
                token: "markup.deleted",
            },
            {
                foreground: "a2bffc",
                fontStyle: "italic",
                token: "markup.changed",
            },
            {
                foreground: "a2bffc",
                fontStyle: "italic",
                token: "meta.diff.header.git",
            },
            {
                foreground: "a2bffc",
                fontStyle: "italic",
                token: "meta.diff.header.from-file",
            },
            {
                foreground: "a2bffc",
                fontStyle: "italic",
                token: "meta.diff.header.to-file",
            },
            {
                foreground: "219186",
                background: "eae3ca",
                token: "markup.inserted",
            },
            {
                foreground: "d3201f",
                token: "other.package.exclude",
            },
            {
                foreground: "d3201f",
                token: "other.remove",
            },
            {
                foreground: "269186",
                token: "other.add",
            },
            {
                foreground: "ff5874",
                token: "constant.language.python",
            },
            {
                foreground: "82aaff",
                token: "variable.parameter.function.python",
            },
            {
                foreground: "82aaff",
                token: "meta.function-call.arguments.python",
            },
            {
                foreground: "b2ccd6",
                token: "meta.function-call.python",
            },
            {
                foreground: "b2ccd6",
                token: "meta.function-call.generic.python",
            },
            {
                foreground: "d6deeb",
                token: "punctuation.python",
            },
            {
                foreground: "addb67",
                token: "entity.name.function.decorator.python",
            },
            {
                foreground: "8eace3",
                token: "source.python variable.language.special",
            },
            {
                foreground: "82b1ff",
                token: "markup.heading.markdown",
            },
            {
                foreground: "c792ea",
                fontStyle: "italic",
                token: "markup.italic.markdown",
            },
            {
                foreground: "addb67",
                fontStyle: "bold",
                token: "markup.bold.markdown",
            },
            {
                foreground: "697098",
                token: "markup.quote.markdown",
            },
            {
                foreground: "80cbc4",
                token: "markup.inline.raw.markdown",
            },
            {
                foreground: "ff869a",
                token: "markup.underline.link.markdown",
            },
            {
                foreground: "ff869a",
                token: "markup.underline.link.image.markdown",
            },
            {
                foreground: "d6deeb",
                token: "string.other.link.title.markdown",
            },
            {
                foreground: "d6deeb",
                token: "string.other.link.description.markdown",
            },
            {
                foreground: "82b1ff",
                token: "punctuation.definition.string.markdown",
            },
            {
                foreground: "82b1ff",
                token: "punctuation.definition.string.begin.markdown",
            },
            {
                foreground: "82b1ff",
                token: "punctuation.definition.string.end.markdown",
            },
            {
                foreground: "82b1ff",
                token: "meta.link.inline.markdown punctuation.definition.string",
            },
            {
                foreground: "7fdbca",
                token: "punctuation.definition.metadata.markdown",
            },
            {
                foreground: "82b1ff",
                token: "beginning.punctuation.definition.list.markdown",
            },
        ],
        colors: {
            "editor.foreground": "#d6deeb",
            //    "editor.background": "#011627",
            "editor.background": bg,
            "editor.selectionBackground": "#5f7e9779",
            // "editor.lineHighlightBackground": "#010E17",
            "editorCursor.foreground": "#80a4c2",
            "editorWhitespace.foreground": "#2e2040",
            "editorIndentGuide.background": "#5e81ce52",
            "editor.selectionHighlightBorder": "#122d42",
        },
    });
};
exports.purescriptSyntax = {
    displayName: "Purescript",
    name: "purescript",
    mimeTypes: ["text/purescript"],
    fileExtensions: ["purs"],
    editorOptions: { tabSize: 2, insertSpaces: true },
    lineComment: "--",
    blockCommentStart: "{-",
    blockCommentEnd: "-}",
    keywords: [
        "case",
        "class",
        "data",
        "derive",
        "do",
        "ado",
        "else",
        "if",
        "import",
        "in",
        "infix",
        "infixl",
        "infixr",
        "instance",
        "let",
        "module",
        "newtype",
        "of",
        "then",
        "type",
        "where",
        "->",
        "|",
        "=>",
        "::",
        "\\",
        "=",
        "|",
        "foreign",
        "forall",
        "∷",
        "∀",
    ],
    extraKeywords: [],
    typeKeywords: [],
    dataStart: ["^data"],
    typeStart: ["class", "instance", "type", "derive"],
    escapes: "\\\\(?:[nrt\\\\\"'\\?]|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{6})",
    symbols0: "[\\$%&\\*\\+@!\\/\\\\\\^~=\\.:\\-\\?]",
    symbols: "(?:@symbols0|[\\|<>])+",
    idchars: "(?:[\\w'])*",
    tokenizer: {
        root: [
            [
                "((?:[A-Z]@idchars\\.)+)([a-z]@idchars)",
                ["identifier.namespace", "identifier"],
            ],
            [
                "((?:[A-Z]@idchars\\.)+)([A-Z]@idchars)",
                ["identifier.namespace", "constructor"],
            ],
            [
                "[a-z]@idchars(?!\\.)",
                {
                    cases: {
                        "@dataStart": { token: "keyword", next: "@type_data" },
                        "@typeStart": { token: "keyword", next: "@type_type" },
                        "@keywords": "keyword",
                        "@extraKeywords": "keyword",
                        "@default": "identifier",
                    },
                },
            ],
            ["[A-Z]@idchars", "type.identifier"],
            ["_@idchars", "identifier.wildcard"],
            ["([a-z]@idchars\\.)+", "identifier.namespace"],
            { include: "@whitespace" },
            ["[{}()\\[\\]]", "@brackets"],
            ["[,`]", "delimiter"],
            ["::(?!@symbols0)", "type.operator", "@type_line"],
            [
                "@symbols",
                {
                    cases: {
                        "@keywords": "keyword.operator",
                        "@extraKeywords": "keyword.operator",
                        "@default": "operator",
                    },
                },
            ],
            ["[0-9]+\\.[0-9]+([eE][\\-+]?[0-9]+)?", "number.float"],
            ["0[xX][0-9a-fA-F]+", "number.hex"],
            ["[0-9]+", "number"],
            ['"([^"\\\\]|\\\\.)*$', "string.invalid"],
            ['"', { token: "string.quote", bracket: "@open", next: "@string" }],
            ["'[^\\\\']'", "string"],
            ["(')(@escapes)(')", ["string", "string.escape", "string"]],
            ["'", "string.invalid"],
            ["^[ ]*#.*", "namespace"],
        ],
        whitespace: [
            ["[ \\r\\n]+", ""],
            ["{-", "comment", "@comment"],
            ["--.*$", "comment"],
        ],
        comment: [
            ["[^\\{\\-]+", "comment"],
            ["{-", "comment", "@push"],
            ["-}", "comment", "@pop"],
            ["[\\{\\-]", "comment"],
        ],
        string: [
            ['[^\\\\"]+', "string"],
            ["@escapes", "string.escape"],
            ["\\\\.", "string.escape.invalid"],
            ['"', { token: "string.quote", bracket: "@close", next: "@pop" }],
        ],
        type_type: [
            ["\\=", "keyword"],
            ["^\\s*(?!\\-\\-|\\{\\-)[^\\s]", { token: "@rematch", next: "@pop" }],
            { include: "@type" },
        ],
        type_data: [
            ["([=|])(\\s*)([A-Z]@idchars)", ["keyword.operator", "", "constructor"]],
            ["^\\s*$", { token: "", next: "@pop" }],
            { include: "@type" },
        ],
        type_line: [
            ["^\\s*(?!\\-\\-|\\{\\-)[_a-z]", { token: "@rematch", next: "@pop" }],
            ["[^=]*=", { token: "@rematch", next: "@pop" }],
            { include: "@type" },
        ],
        type: [
            ["[(\\[]", { token: "@brackets.type" }, "@type_nested"],
            { include: "@type_content" },
        ],
        type_nested: [
            ["[)\\]]", { token: "@brackets.type" }, "@pop"],
            ["[(\\[]", { token: "@brackets.type" }, "@push"],
            [",", "delimiter.type"],
            { include: "@type_content" },
        ],
        type_content: [
            { include: "@whitespace" },
            ["[a-z]\\d*\\b", "type.identifier.typevar"],
            ["_@idchars", "type.identifier.typevar"],
            [
                "((?:[A-Z]@idchars\\.)*)([A-Z]@idchars)",
                ["type.identifier.namespace", "type.identifier"],
            ],
            [
                "((?:[A-Z]@idchars\\.)*)([a-z]@idchars)",
                {
                    cases: {
                        "@typeKeywords": ["type.identifier.namespace", "type.keyword"],
                        "@keywords": { token: "@rematch", next: "@pop" },
                        "@default": ["type.identifier.namespace", "type.identifier"],
                    },
                },
            ],
            ["::|->|[\\.:|]", "type.operator"],
            ["[\\s\\S]", "@rematch", "@pop"],
        ],
    },
};
