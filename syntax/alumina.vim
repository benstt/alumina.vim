" Language:     Alumina
" Description:  Vim syntax file
" Maintainer:   Benjamín García Roqués <benjamingarciaroques@gmail.com>
" Last Change:  October 10, 2024
" For bugs, patches and license go to https://github.com/benstt/aluminavim


if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Syntax definitions {{{1
" Basic keywords {{{2
syn keyword   aluminaConditional switch if else
syn keyword   aluminaRepeat loop while
" `:syn match` must be used to prioritize highlighting `for` keyword.
syn match     aluminaRepeat /\<for\>/
" Highlight `for` keyword in `impl ... for ... {}` statement. This line must
" be put after previous `syn match` line to overwrite it.
syn match     aluminaKeyword /\%(\<impl\>.\+\)\@<=\<for\>/
syn keyword   aluminaRepeat in
syn keyword   aluminaTypedef type nextgroup=aluminaIdentifier skipwhite skipempty
syn keyword   aluminaStructure struct enum nextgroup=aluminaIdentifier skipwhite skipempty
syn keyword   aluminaUnion union nextgroup=aluminaIdentifier skipwhite skipempty contained
syn match aluminaUnionContextual /\<union\_s\+\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*/ transparent contains=aluminaUnion
syn keyword   aluminaOperator    as
syn keyword   aluminaOperator    is
syn keyword   aluminaExistential existential nextgroup=aluminaTypedef skipwhite skipempty contained
syn match aluminaExistentialContextual /\<existential\_s\+type/ transparent contains=aluminaExistential,aluminaTypedef

syn match     aluminaAssert      "\<assert\(\w\)*!" contained
syn match     aluminaPanic       "\<panic\(\w\)*!" contained
syn match     aluminaAsync       "\<async\%(\s\|\n\)\@="
syn keyword   aluminaKeyword     break
syn keyword   aluminaKeyword     continue
syn keyword   aluminaKeyword     defer
syn keyword   aluminaKeyword     when
syn keyword   aluminaKeyword     crate
syn keyword   aluminaKeyword     extern nextgroup=aluminaExternCrate,aluminaObsoleteExternMod skipwhite skipempty
syn keyword   aluminaKeyword     fn nextgroup=aluminaFuncName skipwhite skipempty
syn keyword   aluminaKeyword     impl let
syn keyword   aluminaKeyword     macro
syn keyword   aluminaKeyword     pub nextgroup=aluminaPubScope skipwhite skipempty
syn keyword   aluminaKeyword     return
syn keyword   aluminaKeyword     yield
syn keyword   aluminaKeyword     typeof
syn keyword   aluminaKeyword     mixin
syn keyword   aluminaKeyword     protocol
syn keyword   aluminaSuper       super
syn keyword   aluminaKeyword     where
syn keyword   aluminaKeyword     use nextgroup=aluminaModPath skipwhite skipempty
" FIXME: Scoped impl's name is also fallen in this category
syn keyword   aluminaKeyword     mod trait nextgroup=aluminaIdentifier skipwhite skipempty
syn keyword   aluminaStorage     move mut ref static const
syn match     aluminaDefault     /\<default\ze\_s\+\(impl\|fn\|type\|const\)\>/
syn keyword   aluminaAwait       await
syn match     aluminaKeyword     /\<try\>!\@!/ display

syn keyword aluminaPubScopeCrate crate contained
syn match aluminaPubScopeDelim /[()]/ contained
syn match aluminaPubScope /([^()]*)/ contained contains=aluminaPubScopeDelim,aluminaPubScopeCrate,aluminaSuper,aluminaModPath,aluminaModPathSep,aluminaSelf transparent

syn keyword   aluminaExternCrate crate contained nextgroup=aluminaIdentifier,aluminaExternCrateString skipwhite skipempty
" This is to get the `bar` part of `extern crate "foo" as bar;` highlighting.
syn match   aluminaExternCrateString /".*"\_s*as/ contained nextgroup=aluminaIdentifier skipwhite transparent skipempty contains=aluminaString,aluminaOperator
syn keyword   aluminaObsoleteExternMod mod contained nextgroup=aluminaIdentifier skipwhite skipempty

syn match     aluminaIdentifier  contains=aluminaIdentifierPrime "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained
syn match     aluminaFuncName    "\%(r#\)\=\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained

syn region aluminaMacroRepeat matchgroup=aluminaMacroRepeatDelimiters start="$(" end="),\=[*+]" contains=TOP
syn match aluminaMacroVariable "$\w\+"
syn match aluminaRawIdent "\<r#\h\w*" contains=NONE

" Reserved (but not yet used) keywords {{{2
syn keyword   aluminaReservedKeyword become do priv typeof unsized abstract virtual final override

" Built-in types {{{2
syn keyword   aluminaType        isize usize char bool u8 u16 u32 u64 u128 f32
syn keyword   aluminaType        f64 i8 i16 i32 i64 i128 str Self
syn keyword   aluminaType        void null

" Things from the libstd v1 prelude (src/libstd/prelude/v1.rs) {{{2
" This section is just straight transformation of the contents of the prelude,
" to make it easy to update.

" Reexported core operators {{{3
syn keyword   aluminaTrait       Copy Send Sized Sync
syn keyword   aluminaTrait       Drop Fn FnMut FnOnce

" Reexported functions {{{3
" There’s no point in highlighting these; when one writes drop( or drop::< it
" gets the same highlighting anyway, and if someone writes `let drop = …;` we
" don’t really want *that* drop to be highlighted.
"syn keyword aluminaFunction drop

" Reexported types and traits {{{3
syn keyword aluminaTrait Box
syn keyword aluminaTrait ToOwned
syn keyword aluminaTrait Clone
syn keyword aluminaTrait PartialEq PartialOrd Eq Ord
syn keyword aluminaTrait AsRef AsMut Into From
syn keyword aluminaTrait Default
syn keyword aluminaTrait Iterator Extend IntoIterator
syn keyword aluminaTrait DoubleEndedIterator ExactSizeIterator
syn keyword aluminaEnum Option
syn keyword aluminaEnumVariant Some None
syn keyword aluminaEnum Result
syn keyword aluminaEnumVariant Ok Err
syn keyword aluminaTrait SliceConcatExt
syn keyword aluminaTrait String ToString
syn keyword aluminaTrait Vec

" Other syntax {{{2
syn keyword   aluminaSelf        self
syn keyword   aluminaBoolean     true false

" If foo::bar changes to foo.bar, change this ("::" to "\.").
" If foo::bar changes to Foo::bar, change this (first "\w" to "\u").
syn match     aluminaModPath     "\w\(\w\)*::[^<]"he=e-3,me=e-3
syn match     aluminaModPathSep  "::"

syn match     aluminaFuncCall    "\w\(\w\)*("he=e-1,me=e-1
syn match     aluminaFuncCall    "\w\(\w\)*::<"he=e-3,me=e-3 " foo::<T>();

" This is merely a convention; note also the use of [A-Z], restricting it to
" latin identifiers rather than the full Unicode uppercase. I have not used
" [:upper:] as it depends upon 'noignorecase'
"syn match     aluminaCapsIdent    display "[A-Z]\w\(\w\)*"

syn match     aluminaOperator     display "\%(+\|-\|/\|*\|=\|\^\|&\||\|!\|>\|<\|%\)=\?"
" This one isn't *quite* right, as we could have binary-& with a reference
syn match     aluminaSigil        display /&\s\+[&~@*][^)= \t\r\n]/he=e-1,me=e-1
syn match     aluminaSigil        display /[&~@*][^)= \t\r\n]/he=e-1,me=e-1
" This isn't actually correct; a closure with no arguments can be `|| { }`.
" Last, because the & in && isn't a sigil
syn match     aluminaOperator     display "&&\|||"
" This is aluminaArrowCharacter rather than aluminaArrow for the sake of matchparen,
" so it skips the ->; see http://stackoverflow.com/a/30309949 for details.
syn match     aluminaArrowCharacter display "->"
syn match     aluminaQuestionMark display "?\([a-zA-Z]\+\)\@!"

syn match     aluminaMacro       '\w\(\w\)*!' contains=aluminaAssert,aluminaPanic
syn match     aluminaMacro       '#\w\(\w\)*' contains=aluminaAssert,aluminaPanic

syn match     aluminaEscapeError   display contained /\\./
syn match     aluminaEscape        display contained /\\\([nrt0\\'"]\|x\x\{2}\)/
syn match     aluminaEscapeUnicode display contained /\\u{\%(\x_*\)\{1,6}}/
syn match     aluminaStringContinuation display contained /\\\n\s*/
syn region    aluminaString      matchgroup=aluminaStringDelimiter start=+b"+ skip=+\\\\\|\\"+ end=+"+ contains=aluminaEscape,aluminaEscapeError,aluminaStringContinuation
syn region    aluminaString      matchgroup=aluminaStringDelimiter start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=aluminaEscape,aluminaEscapeUnicode,aluminaEscapeError,aluminaStringContinuation,@Spell
syn region    aluminaString      matchgroup=aluminaStringDelimiter start='b\?r\z(#*\)"' end='"\z1' contains=@Spell

" Match attributes with either arbitrary syntax or special highlighting for
" derives. We still highlight strings and comments inside of the attribute.
syn region    aluminaAttribute   start="#!\?\[" end="\]" contains=@aluminaAttributeContents,aluminaAttributeParenthesizedParens,aluminaAttributeParenthesizedCurly,aluminaAttributeParenthesizedBrackets,aluminaDerive
syn region    aluminaAttributeParenthesizedParens matchgroup=aluminaAttribute start="\w\%(\w\)*("rs=e end=")"re=s transparent contained contains=aluminaAttributeBalancedParens,@aluminaAttributeContents
syn region    aluminaAttributeParenthesizedCurly matchgroup=aluminaAttribute start="\w\%(\w\)*{"rs=e end="}"re=s transparent contained contains=aluminaAttributeBalancedCurly,@aluminaAttributeContents
syn region    aluminaAttributeParenthesizedBrackets matchgroup=aluminaAttribute start="\w\%(\w\)*\["rs=e end="\]"re=s transparent contained contains=aluminaAttributeBalancedBrackets,@aluminaAttributeContents
syn region    aluminaAttributeBalancedParens matchgroup=aluminaAttribute start="("rs=e end=")"re=s transparent contained contains=aluminaAttributeBalancedParens,@aluminaAttributeContents
syn region    aluminaAttributeBalancedCurly matchgroup=aluminaAttribute start="{"rs=e end="}"re=s transparent contained contains=aluminaAttributeBalancedCurly,@aluminaAttributeContents
syn region    aluminaAttributeBalancedBrackets matchgroup=aluminaAttribute start="\["rs=e end="\]"re=s transparent contained contains=aluminaAttributeBalancedBrackets,@aluminaAttributeContents
syn cluster   aluminaAttributeContents contains=aluminaString,aluminaCommentLine,aluminaCommentBlock,aluminaCommentLineDocError,aluminaCommentBlockDocError
syn region    aluminaDerive      start="derive(" end=")" contained contains=aluminaDeriveTrait
" This list comes from src/libsyntax/ext/deriving/mod.rs
" Some are deprecated (Encodable, Decodable) or to be removed after a new snapshot (Show).
syn keyword   aluminaDeriveTrait contained Clone Hash AluminacEncodable AluminacDecodable Encodable Decodable PartialEq Eq PartialOrd Ord Rand Show Debug Default FromPrimitive Send Sync Copy

" dyn keyword: It's only a keyword when used inside a type expression, so
" we make effort here to highlight it only when Alumina identifiers follow it
" (not minding the case of pre-2018 Alumina where a path starting with :: can
" follow).
"
" This is so that uses of dyn variable names such as in 'let &dyn = &2'
" and 'let dyn = 2' will not get highlighted as a keyword.
syn match     aluminaKeyword "\<dyn\ze\_s\+\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)" contains=aluminaDynKeyword
syn keyword   aluminaDynKeyword  dyn contained

" Number literals
syn match     aluminaDecNumber   display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     aluminaHexNumber   display "\<0x[a-fA-F0-9_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     aluminaOctNumber   display "\<0o[0-7_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     aluminaBinNumber   display "\<0b[01_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="

" Special case for numbers of the form "1." which are float literals, unless followed by
" an identifier, which makes them integer literals with a method call or field access,
" or by another ".", which makes them integer literals followed by the ".." token.
" (This must go first so the others take precedence.)
syn match     aluminaFloat       display "\<[0-9][0-9_]*\.\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\|\.\)\@!"
" To mark a number as a normal float, it must have at least one of the three things integral values don't have:
" a decimal point and more numbers; an exponent; and a type suffix.
syn match     aluminaFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)\="
syn match     aluminaFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\(f32\|f64\)\="
syn match     aluminaFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)"

" For the benefit of delimitMate
syn region aluminaLifetimeCandidate display start=/&'\%(\([^'\\]\|\\\(['nrt0\\\"]\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'\)\@!/ end=/[[:cntrl:][:space:][:punct:]]\@=\|$/ contains=aluminaSigil,aluminaLifetime
syn region aluminaGenericRegion display start=/<\%('\|[^[:cntrl:][:space:][:punct:]]\)\@=')\S\@=/ end=/>/ contains=aluminaGenericLifetimeCandidate
syn region aluminaGenericLifetimeCandidate display start=/\%(<\|,\s*\)\@<='/ end=/[[:cntrl:][:space:][:punct:]]\@=\|$/ contains=aluminaSigil,aluminaLifetime

"aluminaLifetime must appear before aluminaCharacter, or chars will get the lifetime highlighting
syn match     aluminaLifetime    display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*"
syn match     aluminaLabel       display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*:"
syn match     aluminaLabel       display "\%(\<\%(break\|continue\)\s*\)\@<=\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*"
syn match   aluminaCharacterInvalid   display contained /b\?'\zs[\n\r\t']\ze'/
" The groups negated here add up to 0-255 but nothing else (they do not seem to go beyond ASCII).
syn match   aluminaCharacterInvalidUnicode   display contained /b'\zs[^[:cntrl:][:graph:][:alnum:][:space:]]\ze'/
syn match   aluminaCharacter   /b'\([^\\]\|\\\(.\|x\x\{2}\)\)'/ contains=aluminaEscape,aluminaEscapeError,aluminaCharacterInvalid,aluminaCharacterInvalidUnicode
syn match   aluminaCharacter   /'\([^\\]\|\\\(.\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'/ contains=aluminaEscape,aluminaEscapeUnicode,aluminaEscapeError,aluminaCharacterInvalid

syn match aluminaShebang /\%^#![^[].*/
syn region aluminaCommentLine                                                  start="//"                      end="$"   contains=aluminaTodo,@Spell
syn region aluminaCommentLineDoc                                               start="//\%(//\@!\|!\)"         end="$"   contains=aluminaTodo,@Spell
syn region aluminaCommentLineDocError                                          start="//\%(//\@!\|!\)"         end="$"   contains=aluminaTodo,@Spell contained
syn region aluminaCommentBlock             matchgroup=aluminaCommentBlock         start="/\*\%(!\|\*[*/]\@!\)\@!" end="\*/" contains=aluminaTodo,aluminaCommentBlockNest,@Spell
syn region aluminaCommentBlockDoc          matchgroup=aluminaCommentBlockDoc      start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=aluminaTodo,aluminaCommentBlockDocNest,aluminaCommentBlockDocAluminaCode,@Spell
syn region aluminaCommentBlockDocError     matchgroup=aluminaCommentBlockDocError start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=aluminaTodo,aluminaCommentBlockDocNestError,@Spell contained
syn region aluminaCommentBlockNest         matchgroup=aluminaCommentBlock         start="/\*"                     end="\*/" contains=aluminaTodo,aluminaCommentBlockNest,@Spell contained transparent
syn region aluminaCommentBlockDocNest      matchgroup=aluminaCommentBlockDoc      start="/\*"                     end="\*/" contains=aluminaTodo,aluminaCommentBlockDocNest,@Spell contained transparent
syn region aluminaCommentBlockDocNestError matchgroup=aluminaCommentBlockDocError start="/\*"                     end="\*/" contains=aluminaTodo,aluminaCommentBlockDocNestError,@Spell contained transparent

" FIXME: this is a really ugly and not fully correct implementation. Most
" importantly, a case like ``/* */*`` should have the final ``*`` not being in
" a comment, but in practice at present it leaves comments open two levels
" deep. But as long as you stay away from that particular case, I *believe*
" the highlighting is correct. Due to the way Vim's syntax engine works
" (greedy for start matches, unlike Alumina's tokeniser which is searching for
" the earliest-starting match, start or end), I believe this cannot be solved.
" Oh you who would fix it, don't bother with things like duplicating the Block
" rules and putting ``\*\@<!`` at the start of them; it makes it worse, as
" then you must deal with cases like ``/*/**/*/``. And don't try making it
" worse with ``\%(/\@<!\*\)\@<!``, either...

syn keyword aluminaTodo contained TODO FIXME XXX NB NOTE SAFETY

" asm! macro {{{2
syn region aluminaAsmMacro matchgroup=aluminaMacro start="\<asm!\s*(" end=")" contains=aluminaAsmDirSpec,aluminaAsmSym,aluminaAsmConst,aluminaAsmOptionsGroup,aluminaComment.*,aluminaString.*

" Clobbered registers
syn keyword aluminaAsmDirSpec in out lateout inout inlateout contained nextgroup=aluminaAsmReg skipwhite skipempty
syn region  aluminaAsmReg start="(" end=")" contained contains=aluminaString

" Symbol operands
syn keyword aluminaAsmSym sym contained nextgroup=aluminaAsmSymPath skipwhite skipempty
syn region  aluminaAsmSymPath start="\S" end=",\|)"me=s-1 contained contains=aluminaComment.*,aluminaIdentifier

" Const
syn region  aluminaAsmConstBalancedParens start="("ms=s+1 end=")" contained contains=@aluminaAsmConstExpr
syn cluster aluminaAsmConstExpr contains=aluminaComment.*,alumina.*Number,aluminaString,aluminaAsmConstBalancedParens
syn region  aluminaAsmConst start="const" end=",\|)"me=s-1 contained contains=aluminaStorage,@aluminaAsmConstExpr

" Options
syn region  aluminaAsmOptionsGroup start="options\s*(" end=")" contained contains=aluminaAsmOptions,aluminaAsmOptionsKey
syn keyword aluminaAsmOptionsKey options contained
syn keyword aluminaAsmOptions pure nomem readonly preserves_flags noreturn nostack att_syntax contained

" Folding rules {{{2
" Trivial folding rules to begin with.
" FIXME: use the AST to make really good folding
syn region aluminaFoldBraces start="{" end="}" transparent fold

if !exists("b:current_syntax_embed")
    let b:current_syntax_embed = 1
    syntax include @AluminaCodeInComment <sfile>:p:h/alumina.vim
    unlet b:current_syntax_embed

    " Currently regions marked as ```<some-other-syntax> will not get
    " highlighted at all. In the future, we can do as vim-markdown does and
    " highlight with the other syntax. But for now, let's make sure we find
    " the closing block marker, because the rules below won't catch it.
    syn region aluminaCommentLinesDocNonAluminaCode matchgroup=aluminaCommentDocCodeFence start='^\z(\s*//[!/]\s*```\).\+$' end='^\z1$' keepend contains=aluminaCommentLineDoc

    " We borrow the rules from alumina’s src/libaluminadoc/html/markdown.rs, so that
    " we only highlight as Alumina what it would perceive as Alumina (almost; it’s
    " possible to trick it if you try hard, and indented code blocks aren’t
    " supported because Markdown is a menace to parse and only mad dogs and
    " Englishmen would try to handle that case correctly in this syntax file).
    syn region aluminaCommentLinesDocAluminaCode matchgroup=aluminaCommentDocCodeFence start='^\z(\s*//[!/]\s*```\)[^A-Za-z0-9_-]*\%(\%(should_panic\|no_run\|ignore\|allow_fail\|alumina\|test_harness\|compile_fail\|E\d\{4}\|edition201[58]\)\%([^A-Za-z0-9_-]\+\|$\)\)*$' end='^\z1$' keepend contains=@AluminaCodeInComment,aluminaCommentLineDocLeader
    syn region aluminaCommentBlockDocAluminaCode matchgroup=aluminaCommentDocCodeFence start='^\z(\%(\s*\*\)\?\s*```\)[^A-Za-z0-9_-]*\%(\%(should_panic\|no_run\|ignore\|allow_fail\|alumina\|test_harness\|compile_fail\|E\d\{4}\|edition201[58]\)\%([^A-Za-z0-9_-]\+\|$\)\)*$' end='^\z1$' keepend contains=@AluminaCodeInComment,aluminaCommentBlockDocStar
    " Strictly, this may or may not be correct; this code, for example, would
    " mishighlight:
    "
    "     /**
    "     ```alumina
    "     println!("{}", 1
    "     * 1);
    "     ```
    "     */
    "
    " … but I don’t care. Balance of probability, and all that.
    syn match aluminaCommentBlockDocStar /^\s*\*\s\?/ contained
    syn match aluminaCommentLineDocLeader "^\s*//\%(//\@!\|!\)" contained
endif

" Default highlighting {{{1
hi def link aluminaDecNumber       aluminaNumber
hi def link aluminaHexNumber       aluminaNumber
hi def link aluminaOctNumber       aluminaNumber
hi def link aluminaBinNumber       aluminaNumber
hi def link aluminaIdentifierPrime aluminaIdentifier
hi def link aluminaTrait           aluminaType
hi def link aluminaDeriveTrait     aluminaTrait

hi def link aluminaMacroRepeatDelimiters   Macro
hi def link aluminaMacroVariable Define
hi def link aluminaSigil         StorageClass
hi def link aluminaEscape        Special
hi def link aluminaEscapeUnicode aluminaEscape
hi def link aluminaEscapeError   Error
hi def link aluminaStringContinuation Special
hi def link aluminaString        String
hi def link aluminaStringDelimiter String
hi def link aluminaCharacterInvalid Error
hi def link aluminaCharacterInvalidUnicode aluminaCharacterInvalid
hi def link aluminaCharacter     Character
hi def link aluminaNumber        Number
hi def link aluminaBoolean       Boolean
hi def link aluminaEnum          aluminaType
hi def link aluminaEnumVariant   aluminaConstant
hi def link aluminaConstant      Constant
hi def link aluminaSelf          Constant
hi def link aluminaFloat         Float
hi def link aluminaArrowCharacter aluminaOperator
hi def link aluminaOperator      Operator
hi def link aluminaKeyword       Keyword
hi def link aluminaDynKeyword    aluminaKeyword
hi def link aluminaTypedef       Keyword " More precise is Typedef, but it doesn't feel right for Alumina
hi def link aluminaStructure     Keyword " More precise is Structure
hi def link aluminaUnion         aluminaStructure
hi def link aluminaExistential   aluminaKeyword
hi def link aluminaPubScopeDelim Delimiter
hi def link aluminaPubScopeCrate aluminaKeyword
hi def link aluminaSuper         aluminaKeyword
hi def link aluminaReservedKeyword Error
hi def link aluminaRepeat        Conditional
hi def link aluminaConditional   Conditional
hi def link aluminaIdentifier    Identifier
hi def link aluminaCapsIdent     aluminaIdentifier
hi def link aluminaModPath       Include
hi def link aluminaModPathSep    Delimiter
hi def link aluminaFunction      Function
hi def link aluminaFuncName      Function
hi def link aluminaFuncCall      Function
hi def link aluminaShebang       Comment
hi def link aluminaCommentLine   Comment
hi def link aluminaCommentLineDoc SpecialComment
hi def link aluminaCommentLineDocLeader aluminaCommentLineDoc
hi def link aluminaCommentLineDocError Error
hi def link aluminaCommentBlock  aluminaCommentLine
hi def link aluminaCommentBlockDoc aluminaCommentLineDoc
hi def link aluminaCommentBlockDocStar aluminaCommentBlockDoc
hi def link aluminaCommentBlockDocError Error
hi def link aluminaCommentDocCodeFence aluminaCommentLineDoc
hi def link aluminaAssert        PreCondit
hi def link aluminaPanic         PreCondit
hi def link aluminaMacro         Macro
hi def link aluminaType          Type
hi def link aluminaTodo          Todo
hi def link aluminaAttribute     PreProc
hi def link aluminaDerive        PreProc
hi def link aluminaDefault       StorageClass
hi def link aluminaStorage       StorageClass
hi def link aluminaObsoleteStorage Error
hi def link aluminaLifetime      Special
hi def link aluminaLabel         Label
hi def link aluminaExternCrate   aluminaKeyword
hi def link aluminaObsoleteExternMod Error
hi def link aluminaQuestionMark  Special
hi def link aluminaAsync         aluminaKeyword
hi def link aluminaAwait         aluminaKeyword
hi def link aluminaAsmDirSpec    aluminaKeyword
hi def link aluminaAsmSym        aluminaKeyword
hi def link aluminaAsmOptions    aluminaKeyword
hi def link aluminaAsmOptionsKey aluminaAttribute

" Other Suggestions:
" hi aluminaAttribute ctermfg=cyan
" hi aluminaDerive ctermfg=cyan
" hi aluminaAssert ctermfg=yellow
" hi aluminaPanic ctermfg=red
" hi aluminaMacro ctermfg=magenta

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "alumina"

" vim: set et sw=4 sts=4 ts=8:
