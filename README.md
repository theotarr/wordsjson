<p align="center">
    <img src="https://www.latindictionary.io/static/assets/img/favicon/mstile-150x150.png" width=200 height=200/>
</p>
<h1 align="center">
    JSON Wrapper WORDS
</h1>

### Description
- a wrapper for the wordsxml program provided by Alpheios
- converts XML response to a JSON response and standardizes the output
- Dockerized to allow portability and ease of use

### Example Response
Query: "puer"

``` json
{
  "words": {
    "word": [
      {
        "form": "puer",
        "entry": [
          {
            "infl": [
              {
                "term": {
                  "stem": "puer",
                  "suff": null
                },
                "pofs": "noun",
                "decl": "2nd",
                "var": "3rd",
                "case": "nominative",
                "num": "singular",
                "gend": "masculine"
              },
              {
                "term": {
                  "stem": "puer",
                  "suff": null
                },
                "pofs": "noun",
                "decl": "2nd",
                "var": "3rd",
                "case": "vocative",
                "num": "singular",
                "gend": "masculine"
              }
            ],
            "dict": [
              {
                "hdwd": "puer, pueri",
                "pofs": "noun",
                "decl": "2nd",
                "gend": "masculine",
                "age": null,
                "area": null,
                "geo": null,
                "freq": "very frequent",
                "src": null
              }
            ],
            "mean": [
              "boy, lad, young man; servant; (male) child; [a puere => from boyhood];"
            ]
          }
        ]
      }
    ]
  }
}
```

### Taglist
List of data tags used in a JSON response.

```
age   :  age
area  :  subject area
case  :  case (nominative, vocative, ...)
comp  :  comparison (positive, comparative, superlative)
conj  :  conjugation (1st, 2nd, ...)
decl  :  declension (1st, 2nd, ...)
dict  :  dictionary
entry :  entry
freq  :  frequency
gend  :  gender
geo   :  geographic area
hdwd  :  dictionary headword(s)
infl  :  inflected form
kind  :  verb kind (transitive, intransitive, ...)
mean  :  meaning
mood  :  mood (indicative, subjunctive, ...)
note  :  note
num   :  number (singular, plural)
pers  :  person (1st, 2nd, 3rd)
pofs  :  part of speech
phrase:  input text
sort  :  numeral sort (cardinal, ordinal, ...)
src   :  source
stem  :  stem
suff  :  suffix
tense :  tense (present, imperfect, ...)
term  :  term
var   :  variant (1st, 2nd, ...)
voice :  voice (active, passive)
word  :  word
xmpl  :  example
```