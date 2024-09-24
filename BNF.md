_italic text_
**bold text**
~~strikethrough~~
[link](https://deepneuron.org)
`inline code`
test[^1] footnote (no whitespace between ^ and integer)

can i modify a == heading with more #. assuming no

```BNF
<mdText> ::= <thing>
<thing> ::= <thing> <thing> | <block> 
<block> ::= <blockContent> '\n' | ''
<blockContent> ::= <image> | <footnoteRef> | <freeText> | <heading> | <blockQuote> | <code> | <orderedList> | <table> | <blank>

<image> ::= '!' <AltText> '(' <imgURL> <captionText>')'
<AltText> ::= '[' <string> ']'
<imgURL> ::= <string> <whiteSpace>
<captionText> ::= '"' <string> '"'

<footnoteRef> ::= '[^' Integer ']:' <optionalWhiteSpace> <string>

<freeText> ::= <freeText> <modText> <freeText> | <string> 
<modText> ::= <italic> | <bold> | <strikeThrough> | <link> | <inlineCode> | <footnote>
<italic> ::= '_' <string> '_'
<bold> ::= '**' <string> '**'
<strikeThrough> ::= '~~' <string> '~~'
<link> ::=  <linkText> <linkURL>
<linkText> ::= '[' <string> ']'
<linkURL> ::= '('<string>')' // does this need validation
<inlineCode> ::= '`' <string> '`'
<footnote> ::= '[^' Integer ']'
<optionalWhiteSpace> ::= '' | <whiteSpace>
<whiteSpace> ::= <whiteSpace><whiteSpace> | ' ' | '\t' | '\r' | '\f' | '\v' 

<heading> ::= <heading1> | <heading2> | <heading3> | <heading4> | <heading5> | <heading6>
<heading6> ::= '#' <heading5>
<heading5> ::= '#' <heading4>
<heading4> ::= '#'<heading3> 
<heading3> ::= '###' <whiteSpace> <freeText>
<heading2> ::= '##' <whiteSpace> <freeText> | <freeText> '\n' <manyDashes>
<manyDashes> ::= '-' <dashes>
<dashes> :: <dashes> <dashes> | '-' 
<heading1> ::= '#' <whiteSpace> <freeText> | <freeText> '\n' <manyEquals>
<manyEquals> ::= '=' <equals>
<equals> ::= <equals> <equals> | '='
<blockQuote> ::=
<code> ::=
<orderedList> ::=
<table> ::=
<blank> ::= <blank> <blank> | '\n' // to handle lots of entering. might get switched out for tokens.



```