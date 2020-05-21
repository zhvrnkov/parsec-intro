import ParseC

main = undefined

whiteSpace = many1 . anyOf $ show ['\0', ' ', '\t']
comment = (\(x, y) -> x ++ y) |>> (commentBegin .>>. anyLine)
comments = unlines |>> (many comment)
anyLine = (concat |>> (many (anyWord <|> whiteSpace <|> commentBegin))) .>> parser '\n'
commentBegin = psequence $ parsers "//"
enum = psequence (parsers "enum")
space = psequence $ parsers " "
anyWord = many1 . anyOf $ show (['a'..'z'] ++ ['A'..'Z'])
enumDeclaration = psequence [enum, space, anyWord]

testData = unlines ["//",
                    "//comment",
                    "// comment with",
                    "enum Foo {",
                    "case first(one: Int)",
                    "enum NestedTest {",
                    "case just(one: Int)",
                    "}",
                    "case second",
                    "enum NestedTest {",
                    "case just(one: Int)",
                    "}",
                    "case third(one: Int)",
                    "}"]
data SwiftEnum = SwiftEnum String [EnumMembers]           
data EnumMembers = NestedEnum SwiftEnum | Cases [String]

parseSwiftEnum :: String -> [EnumMembers]
parseSwiftEnum = undefined

test = psequence [comments, enum, space, anyWord] $ testData
