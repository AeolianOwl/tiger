type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ref 0
val inStr = ref false
val strBuf = ref ""
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = (
    let val pos = hd(!linePos) in
		if !commentDepth > 0 then ErrorMsg.error pos ("unterminated comment") else ();
		if !inStr = true then ErrorMsg.error pos ("unterminated string") else ();
		Tokens.EOF(pos,pos)
	end
)


%% 
%s COMMENT STRING;
alpha=[A-Za-z];
num=[0-9];
alphaNum=alpha|num;
alphaNumU=alpha|num|_;
%%

\n			=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL>" "		=> (continue());
<INITIAL>\t			=> (continue());
<INITIAL>"&"		=> (Tokens.AND(yypos,yypos+1));
<INITIAL>"|"		=> (Tokens.OR(yypos,yypos+1));
<INITIAL>"+"		=> (Tokens.PLUS(yypos,yypos+1));
<INITIAL>"-"		=> (Tokens.MINUS(yypos,yypos+1));
<INITIAL>"*"		=> (Tokens.TIMES(yypos,yypos+1));
<INITIAL>"/"		=> (Tokens.DIVIDE(yypos,yypos+1));
<INITIAL>","		=> (Tokens.COMMA(yypos,yypos+1));
<INITIAL>";"		=> (Tokens.SEMICOLON(yypos,yypos+1));
<INITIAL>"."		=> (Tokens.DOT(yypos,yypos+1));
<INITIAL>":="		=> (Tokens.ASSIGN(yypos,yypos+2));
<INITIAL>":"		=> (Tokens.COLON(yypos,yypos+1));
<INITIAL>"["		=> (Tokens.LBRACK(yypos,yypos+1));
<INITIAL>"]"		=> (Tokens.RBRACK(yypos,yypos+1));
<INITIAL>"("		=> (Tokens.LPAREN(yypos,yypos+1));
<INITIAL>")"		=> (Tokens.RPAREN(yypos,yypos+1));
<INITIAL>"{"		=> (Tokens.LBRACE(yypos,yypos+1));
<INITIAL>"}"		=> (Tokens.RBRACE(yypos,yypos+1));
<INITIAL>"="		=> (Tokens.EQ(yypos,yypos+1));
<INITIAL>"<>"		=> (Tokens.NEQ(yypos,yypos+2));
<INITIAL>"<="		=> (Tokens.LE(yypos,yypos+2));
<INITIAL>"<"		=> (Tokens.LT(yypos,yypos+1));
<INITIAL>">="		=> (Tokens.GE(yypos,yypos+2));
<INITIAL>">"		=> (Tokens.GT(yypos,yypos+1));
<INITIAL>while		=> (Tokens.WHILE(yypos,yypos+5));
<INITIAL>for		=> (Tokens.FOR(yypos,yypos+3));
<INITIAL>to			=> (Tokens.TO(yypos,yypos+2));
<INITIAL>break		=> (Tokens.BREAK(yypos,yypos+5));
<INITIAL>let		=> (Tokens.LET(yypos,yypos+3));
<INITIAL>in			=> (Tokens.IN(yypos,yypos+2));
<INITIAL>end		=> (Tokens.END(yypos,yypos+3));
<INITIAL>function	=> (Tokens.FUNCTION(yypos,yypos+8));
<INITIAL>var		=> (Tokens.VAR(yypos,yypos+3));
<INITIAL>type		=> (Tokens.TYPE(yypos,yypos+4));
<INITIAL>array		=> (Tokens.ARRAY(yypos,yypos+5));
<INITIAL>if			=> (Tokens.IF(yypos,yypos+2));
<INITIAL>then		=> (Tokens.THEN(yypos,yypos+4));
<INITIAL>else		=> (Tokens.ELSE(yypos,yypos+4));
<INITIAL>do			=> (Tokens.DO(yypos,yypos+2));
<INITIAL>of			=> (Tokens.OF(yypos,yypos+2));
<INITIAL>nil		=> (Tokens.NIL(yypos,yypos+3));
<INITIAL>{alpha}+{alphaNumU}*	=> (Tokens.ID(yytext, yypos, yypos+size(yytext)));
<INITIAL>{num}+		=> (
	let val number = (Int.fromString yytext) handle _ => (ErrorMsg.error yypos ("integer overflow"); SOME 0)
	in Tokens.INT(valOf number, yypos, yypos+size(yytext))
	end
);

<INITIAL>\"			=> (YYBEGIN STRING; inStr := true; strBuf := ""; continue());
<STRING>\\\"		=> (strBuf := !strBuf ^ yytext; continue());
<STRING>\"			=> (
	YYBEGIN INITIAL;
	inStr := false;
	case (String.fromString (!strBuf)) of
		SOME x		=> (Tokens.STRING(x, yypos, yypos+size(!strBuf)))
		| None		=> (ErrorMsg.error yypos ("invalid control code"); continue())
);
<STRING>.			=> (strBuf := !strBuf ^ yytext; continue());

"/*"				=> (YYBEGIN COMMENT; commentDepth := !commentDepth+1; continue());
<COMMENT>"*/"		=> (commentDepth := !commentDepth-1; if !commentDepth = 0 then YYBEGIN INITIAL else YYBEGIN COMMENT; continue());
<COMMENT>.			=> (continue());

<INITIAL>.			=> (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());