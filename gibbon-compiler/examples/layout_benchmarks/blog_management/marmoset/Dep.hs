module Dep where

import Gibbon.Prelude
import Gibbon.PList
import Gibbon.Vector

type Text   = Vector Char

--type Target = (Text, Text)
--type Attr   = (Text, (PList Text), (PList (Text, Text)))
--type Format = Format Text

-- For simplicity, we are assuming for this benchmark that data Inline is tokenized at the "word" level.
-- Therefore, The Base case where "Text" is used is going to be a single word, i.e, "Str Text".
data Inline =     Str Text
                | Emph (PList Inline)
             -- | Underline (PList Inline)
             -- | Strong (PList Inline)
             -- | Strikeout (PList Inline)
             -- | Superscript (PList Inline)
             -- | Subscript (PList Inline)
             -- | SmallCaps (PList Inline)
             -- | Quoted QuoteType (PList Inline)
             -- | Cite [Citation] (PList Inline)
             -- | Code Attr Text
                | Space
             -- | SoftBreak
             -- | LineBreak
             -- | Math MathType Text
             -- | RawInline Format Text
             -- | Link Attr (PList Inline) Target
             -- | Image Attr (PList Inline) Target
             -- | Note (PList Block)
             -- | Span Attr (PList Inline)
                deriving (Show)  

data Block =      Plain (PList Inline)
             -- | Para  (PList Inline)
             -- | LineBlock (PList (PList Inline))
             -- | CodeBlock Attr Text
             -- | RawBlock Format Text
             -- | BlockQuote (PList Block)
             -- | OrderedList ListAttributes [[Block]]
             -- | BulletList (PList (PList Block))
             -- | DefinitionList PList ( PList Inline , PList (PList Block) ) ---> This is resulting in a compile time error (TODO: DEBUG)
             -- | Header Int Attr (PList Inline)
             -- | HorizontalRule
             -- | Table Attr Caption [ColSpec] TableHead [TableBody] TableFoot
             -- | Div Attr (PList Block)
                | Null
                deriving (Show)

-- Define Blog elements
data BlogHeader  = Header Text
data BlogId      = ID Int
data BlogAuthor  = Author Text
data BlogDate    = Date Text
data BlogContent = Content Block
data BlogTags    = TagList (PList Text)

-- Define packed Blog data Type/s, we can arrange the fields here to change their relative ordering. 
data Blog =   End 
            | Layout1 (BlogHeader) (BlogId) (BlogAuthor) (BlogDate) (BlogContent) (BlogTags) (Blog)
            | Layout2 (BlogContent) (BlogTags) (Blog) (BlogHeader) (BlogId) (BlogAuthor) (BlogDate) 
            | Layout3 (BlogTags) (Blog) (BlogContent) (BlogHeader) (BlogId) (BlogAuthor) (BlogDate)
            | Layout4 (BlogTags) (BlogContent) (Blog) (BlogHeader) (BlogId) (BlogAuthor) (BlogDate)
            | Layout5 (Blog) (BlogTags) (BlogContent) (BlogHeader) (BlogId) (BlogAuthor) (BlogDate)
            | Layout6 (BlogHeader) (BlogId) (BlogAuthor) (BlogDate) (BlogContent) (Blog) (BlogTags)
            | Layout7 (Blog) (BlogContent) (BlogHeader) (BlogId) (BlogAuthor) (BlogDate) (BlogTags)
            | Layout8 (BlogContent) (Blog) (BlogId) (BlogAuthor) (BlogDate) (BlogHeader) (BlogTags)
            deriving (Show)


-- Data structures for a Fully Inverted Index table 
-- LocationInfo = (Doc id or address, location/address in document)
-- Mapping      = (keyword as text, List of Docs as LocationInfo tuples)
-- But maybe, Mapping and Location Info should be packed data as well? If we are to run multiple passes on them?

-- type LocationInfo  = (Int, Int) 
-- type Mapping       = (Text, PList LocationInfo)
-- type InvertedTable = PList Mapping

--mkChar :: Int -> Char 
--mkChar val = 'a'

-- Get a random word, based on an Int option
--getRandomString :: Int -> Text
--getRandomString option = 
--    if option == 0 then 
--        let str :: Text
--            str = generate 100 mkChar
--            in str
--    else if option == 1 then
--        let str :: Text 
--             str = generate 100 mkChar
--             in str
--     else if option == 2 then
--         let str :: Text 
--             str = generate 100 mkChar
--             in str
--     else if option == 3 then
--         let str :: Text 
--             str = generate 100 mkChar
--             in str
--     else if option == 4 then
--         let str :: Text 
--             str = generate 100 mkChar
--             in str
--     else if option == 5 then
--         let str :: Text 
--             str = generate 100 mkChar
--             in str
--     else if option == 6 then
--         let str :: Text 
--             str = generate 100 mkChar
--             in str
--     else if option == 7 then
--         let str :: Text
--             str = generate 100 mkChar
--             in str
--     else let str :: Text 
--              str = generate 100 mkChar
--              in str

-- Make an Inline type, option chooses what kind of Inline data type we are creating
-- This will Purposefully make Inline lists at a depth of recursion 1, this can be modified to increase the depth of recursion.
-- This function crates the recursive fields. 
{-mkInline :: Int -> Inline
mkInline option = 
   if option == 0 then (Emph (mkInlineList 1 1))                            --- arg
   else if option == 1 then (Underline (mkInlineList 1 1))                  --- arg
   else if option == 2 then (Strong (mkInlineList 1 1))                     --- arg
   else if option == 3 then (Strikeout (mkInlineList 1 1))                  --- arg 
   else if option == 4 then (Superscript (mkInlineList 1 1))                --- arg
   else if option == 5 then (Subscript (mkInlineList 1 1))                  --- arg
   else if option == 6 then (SmallCaps (mkInlineList 1 1))                  --- arg    
   else (Note (mkBlockList 1 1))      -}                                      --- arg


-- Make an Inline type, option chooses what kind of Inline data type we are creating
-- This creates all the base cases for the inline type 
-- mkInlineBaseCase :: Int -> Inline
-- mkInlineBaseCase option = 
--    if option == 0 then (Str (getRandomString (mod rand 9)))   -- get a random word
--    else if option == 1 then Space
--    else if option == 2 then SoftBreak
--    else LineBreak 

-- Make a list of Inline data Type.
-- mkInlineList :: Int -> Int -> (PList Inline)
-- mkInlineList length base = 
--    if length <= 0 then Nil 
--    -- If its not base case, then don't stop recursion. 
--    else if (base == 0) then 
--       let item = (mkInline (mod rand 8))
--           rst  = (mkInlineList (length - 1) base)
--           in Cons item rst
--    -- If its  base case, then stop recursion in Inline data type and only add base cases. 
--    else let item = (mkInlineBaseCase (mod rand 4))
--             rst  = mkInlineList (length - 1) base 
--          in Cons item rst

-- Make a list of blocks
-- mkBlockList :: Int -> Int -> (PList Block)
-- mkBlockList length base = 
--    if length <= 0 then Nil
--    else if (base == 0) then
--       let item = (mkBlock (mod rand 3))
--           rst  = (mkBlockList (length - 1) base)
--       in Cons item rst
--    else let item = (mkBlockBaseCase (mod rand 2))
--             rst  = (mkBlockList (length - 1) base) 
--       in Cons item rst

-- Make a Block data type with random data, make depth of recursion to 1 for now
-- mkBlock :: Int -> Block
-- mkBlock option = 
--    if option == 0 then (Plain (mkInlineList 1 1))                    --- arg
--    else if option == 1 then (Para (mkInlineList 1 1))                --- arg
--    else (BlockQuote (mkBlockList 1 1))                               --- arg 

-- Base case for make Block
-- mkBlockBaseCase :: Int -> Block 
-- mkBlockBaseCase option = 
--    if option == 0 then HorizontalRule
--    else Null

-- A function to make a list of tags each filled with some random tags
-- mkTagList :: Int -> (PList Text)
-- mkTagList length = 
--    if length <= 0 then Nil
--    else let elem = (getRandomString (mod rand 9))
--             rst  = mkTagList (length - 1)
--           in Cons elem rst


-- Utility Functions to make Blogs and its Elements.
mkBlogHeader :: Text -> BlogHeader
mkBlogHeader text = Header text 

mkBlogID :: Int -> BlogId 
mkBlogID val = ID val 

mkBlogAuthor :: Text -> BlogAuthor 
mkBlogAuthor text = Author text

mkBlogDate :: Text -> BlogDate 
mkBlogDate text = Date text 

mkBlogContent :: Block -> BlogContent 
mkBlogContent block = Content block 

mkBlogTags :: (PList Text) -> BlogTags 
mkBlogTags taglist = TagList taglist            
                               

-- mkBlogs_layout1 :: Int -> Int -> Int -> Blog
-- mkBlogs_layout1 length id tag_length =
--    if length <= 0 then End
--    else 
--       let header  = mkBlogHeader (getRandomString (mod rand 9))
--           blogID  = mkBlogID id
--           author  = mkBlogAuthor (getRandomString (mod rand 9))
--           date    = mkBlogDate (getRandomString (mod rand 9))
--           content = mkBlogContent (mkBlock (mod rand 2))
--           tags    = mkBlogTags (mkTagList 100)
--           rst     = mkBlogs_layout1 (length - 1) (id+1) tag_length 
--          in Layout1 header blogID author date content tags rst


-- mkBlogs_layout1 :: Int -> Int -> Int -> Blog
-- mkBlogs_layout1 length id tag_length =
--    if length <= 0 then End
--    else 
--       let header  = Header (getRandomString (mod rand 9))
--           blogID  = ID id
--           author  = Author (getRandomString (mod rand 9))
--           date    = Date (getRandomString (mod rand 9))
--           content = Content (mkBlock (mod rand 2))
--           tags    = TagList (mkTagList tag_length)
--           rst     = mkBlogs_layout1 (length - 1) (id+1) tag_length 
--          in Layout1 header blogID author date content tags rst
-- 
-- 
-- mkBlogs_layout2 :: Int -> Int -> Int -> Blog
-- mkBlogs_layout2 length id tag_length =
--    if length <= 0 then End
--    else 
--       let content = Content (mkBlock (mod rand 2))
--           tags    = TagList (mkTagList tag_length)  
--           rst     = mkBlogs_layout2 (length - 1) (id+1) tag_length
--           header  = Header (getRandomString (mod rand 9))
--           blogID  = ID id
--           author  = Author (getRandomString (mod rand 9))
--           date    = Date (getRandomString (mod rand 9))                  
--          in Layout2 content tags rst header blogID author date 
-- 
-- mkBlogs_layout3 :: Int -> Int -> Int -> Blog
-- mkBlogs_layout3 length id tag_length =
--    if length <= 0 then End
--    else 
--       let tags    = TagList (mkTagList tag_length)
--           rst     = mkBlogs_layout3 (length - 1) (id+1) tag_length
--           content = Content (mkBlock (mod rand 2))          
--           header  = Header (getRandomString (mod rand 9))
--           blogID  = ID id
--           author  = Author (getRandomString (mod rand 9))
--           date    = Date (getRandomString (mod rand 9))           
--          in Layout3 tags rst content header blogID author date
-- 
-- mkBlogs_layout4 :: Int -> Int -> Int -> Blog
-- mkBlogs_layout4 length id tag_length =
--    if length <= 0 then End
--    else 
--       let tags    = TagList (mkTagList tag_length)
--           content = Content (mkBlock (mod rand 2))
--           rst     = mkBlogs_layout4 (length - 1) (id+1) tag_length          
--           header  = Header (getRandomString (mod rand 9))          
--           blogID  = ID id
--           author  = Author (getRandomString (mod rand 9))
--           date    = Date (getRandomString (mod rand 9))           
--          in Layout4 tags content rst header blogID author date
-- 
-- mkBlogs_layout5 :: Int -> Int -> Int -> Blog
-- mkBlogs_layout5 length id tag_length =
--    if length <= 0 then End
--    else 
--       let rst     = mkBlogs_layout5 (length - 1) (id+1) tag_length
--           tags    = TagList (mkTagList tag_length)
--           content = Content (mkBlock (mod rand 2))                       
--           header  = Header (getRandomString (mod rand 9))
--           blogID  = ID id
--           author  = Author (getRandomString (mod rand 9))
--           date    = Date (getRandomString (mod rand 9))           
--          in Layout5 rst tags content header blogID author date
-- 
-- mkBlogs_layout6 :: Int -> Int -> Int -> Blog
-- mkBlogs_layout6 length id tag_length =
--    if length <= 0 then End
--    else 
--       let header  = Header (getRandomString (mod rand 9))          
--           blogID  = ID id
--           author  = Author (getRandomString (mod rand 9))
--           date    = Date (getRandomString (mod rand 9))
--           content = Content (mkBlock (mod rand 2))                
--           rst     = mkBlogs_layout6 (length - 1) (id+1) tag_length
--           tags    = TagList (mkTagList tag_length)        
--          in Layout6 header blogID author date content rst tags
-- 
-- mkBlogs_layout7 :: Int -> Int -> Int -> Blog
-- mkBlogs_layout7 length id tag_length =
--    if length <= 0 then End
--    else 
--       let rst     = mkBlogs_layout7 (length - 1) (id+1) tag_length
--           content = Content (mkBlock (mod rand 2))                                 
--           header  = Header (getRandomString (mod rand 9))
--           blogID  = ID id
--           author  = Author (getRandomString (mod rand 9))
--           date    = Date (getRandomString (mod rand 9))
--           tags    = TagList (mkTagList tag_length)           
--          in Layout7 rst content header blogID author date tags
-- 
-- -- Layout8 (BlogContent) (Blog) (BlogTags) (BlogId) (BlogAuthor) (BlogDate) (BlogHeader)
-- mkBlogs_layout8 :: Int -> Int -> Int -> Blog 
-- mkBlogs_layout8 length id tag_length = 
--    if length <= 0 then End 
--    else 
--       let content = Content (mkBlock (mod rand 2))
--           rst     = mkBlogs_layout8 (length - 1) (id+1) tag_length
--           id      = ID id 
--           author  = Author (getRandomString (mod rand 9))
--           date    = Date (getRandomString (mod rand 9))
--           header  = Header (getRandomString (mod rand 9))
--           tags    = TagList (mkTagList tag_length)
--        in Layout8 content rst id author date header tags
          

-- Function to make a text string of some length. 
-- First argument takes Length, returns a text string.
-- mkText :: Int -> Text
-- mkText length = 
--     let string :: Text
--         string = generate length (\i -> i)
--     in string

-- End functions that will be used to create data structures with random data in it.

-- Begin functions to search the content for a particular keyword.
-- If keyword is present highlight that keyword (Emph) in the content 
-- For now to make things simple, consider a keyword in represented via an Int.

-- search for a particular keyword (Int) in a Text blob (Vector Int) 
-- TODO: Replace Text = Vector Int to Vector Char
-- searchKeywordText :: Int -> Text -> Bool
-- searchKeywordText keyword text = 
--    let len = length text in 
--    if len <= 0 then False
--    else let front  = head text
--             sliced = slice 1 (len - 1) text
--          in 
--          if front == keyword then (True || searchKeywordText keyword sliced)
--          else (False || searchKeywordText keyword sliced)  

-- Maybe use this function to implement such that you can search a keyword in a string of words ? 
-- searchKeywordText :: Text -> Text -> Bool
-- searchKeywordText keyword text = False

-- Function to compare two words, each represented by Vector Char. 
compareWord :: Text -> Text -> Bool
compareWord word1 word2 = 
   let len1        = length word1 
       len2        = length word2
       compare_len = if (len1 == len2) then True else False     
   in if (compare_len) then (cmp 0 len1 word1 word2) else False

-- Compare 2 Vector Char (Text) or words for equality if their length is the same. 
cmp :: Int -> Int -> Vector Char -> Vector Char -> Bool
cmp start end word1 word2 =
   if (start < end) then 
      let a       = nth word1 start
          b       = nth word2 start
          eq      = if (a *==* b) then True else False 
          recurse = cmp (start+1) end word1 word2
         in (eq && recurse) 
   else True

-- Search a TagList (PList Text) for some keyword
searchTagList :: Text -> PList Text -> Bool 
searchTagList keyword taglist = case taglist of 
   Nil -> False 
   Cons word rst -> (compareWord keyword word) || (searchTagList keyword rst) 

-- Search if a particular Tag exists in the Tag list of the blog
searchKeywordInBlogsTagList :: Text -> Blog -> Bool
searchKeywordInBlogsTagList keyword blog = 
   case blog of 
      End -> False 
      Layout1 header id author date content tags rst -> case tags of 
                                                             TagList list -> (searchTagList keyword list)
      Layout2 content tags rst header id author date -> case tags of 
                                                             TagList list -> (searchTagList keyword list)
      Layout3 tags rst content header id author date -> case tags of 
                                                             TagList list -> (searchTagList keyword list)
      Layout4 tags content rst header id author date -> case tags of 
                                                             TagList list -> (searchTagList keyword list)

-- Filter Blogs with a particular keyword in the TagList of the Blog
filterBlogsBasedOnKeywordInTagList :: Text -> Blog -> Blog
filterBlogsBasedOnKeywordInTagList keyword blogs = case blogs of
      End                                             -> End
      Layout1 header id author date content tags rst  -> case tags of 
         TagList list -> let exists = searchTagList keyword list 
                             rst'   = filterBlogsBasedOnKeywordInTagList keyword rst
                           in if (exists) then Layout1 header id author date content (copyPacked tags) rst'
                              else rst'
      Layout2 content tags rst header id author date   -> case tags of 
         TagList list -> let exists = searchTagList keyword list 
                             rst'   = filterBlogsBasedOnKeywordInTagList keyword rst
                           in if (exists) then Layout2 content (copyPacked tags) rst' header id author date
                              else rst'
      Layout3 tags rst content header id author date   -> case tags of 
         TagList list -> let exists = searchTagList keyword list 
                             rst'   = filterBlogsBasedOnKeywordInTagList keyword rst 
                           in if (exists) then Layout3 (copyPacked tags) rst' content header id author date
                              else rst'
      Layout4 tags content rst header id author date   -> case tags of 
         TagList list -> let exists = searchTagList keyword list 
                             rst'   = filterBlogsBasedOnKeywordInTagList keyword rst
                           in if (exists) then Layout4 (copyPacked tags) content rst' header id author date
                              else rst' 
      Layout5 rst tags content header id author date  -> case tags of
         TagList list -> let exists = searchTagList keyword list 
                             rst'   = filterBlogsBasedOnKeywordInTagList keyword rst
                           in if (exists) then Layout5 rst' (copyPacked tags) content header id author date
                              else rst'
      Layout6 header id author date content rst tags  -> case tags of
         TagList list -> let exists = searchTagList keyword list 
                             rst'   = filterBlogsBasedOnKeywordInTagList keyword rst
                           in if (exists) then Layout6 header id author date content rst' tags
                              else rst'
      Layout7 rst content header id author date tags  -> case tags of
         TagList list -> let exists = searchTagList keyword list 
                             rst'   = filterBlogsBasedOnKeywordInTagList keyword rst
                           in if (exists) then Layout7 rst' content header id author date tags
                              else rst'

-- Tell if a particular keyword exists in a Block data type or not
isKeywordPresentInBlock :: Text -> Block -> Bool
isKeywordPresentInBlock keyword contentBlock = 
   case contentBlock of 
      Plain list_inline      -> (searchInlineListForKeyword keyword list_inline)
      --Para  list_inline      -> (searchInlineListForKeyword keyword list_inline)
      --BlockQuote list_block  -> (searchBlockListForKeyword keyword list_block)
      --HorizontalRule         -> False
      Null                   -> False 

-- Tell if a particular keyword exists in an inline data type or not. (search a Inline)
isKeywordPresentInline :: Text -> Inline -> Bool
isKeywordPresentInline keyword inline = 
   case inline of 
      Str text                -> (compareWord keyword text)
      Emph list_inline        -> (searchInlineListForKeyword keyword list_inline)
      --Underline list_inline   -> (searchInlineListForKeyword keyword list_inline)
      --Strong list_inline      -> (searchInlineListForKeyword keyword list_inline)
      --Strikeout list_inline   -> (searchInlineListForKeyword keyword list_inline)
      --Superscript list_inline -> (searchInlineListForKeyword keyword list_inline)
      --Subscript list_inline   -> (searchInlineListForKeyword keyword list_inline) 
      --SmallCaps list_inline   -> (searchInlineListForKeyword keyword list_inline) 
      Space                   -> False
      --SoftBreak               -> False 
      --LineBreak               -> False 
      --Note list_block         -> (searchBlockListForKeyword keyword list_block)

-- Search a block list for a particular keyword
searchBlockListForKeyword :: Text -> PList Block -> Bool 
searchBlockListForKeyword keyword block_list = 
   case block_list of 
      Nil             -> False
      Cons block rst  -> (isKeywordPresentInBlock keyword block) || (searchBlockListForKeyword keyword rst) 

-- Search an Inline list for a particular keyword
searchInlineListForKeyword :: Text -> PList Inline -> Bool
searchInlineListForKeyword keyword inline_list = 
   case inline_list of 
      Nil                -> False 
      Cons inline rst    -> (isKeywordPresentInline keyword inline) || (searchInlineListForKeyword keyword rst)

-- Search blogs for a particular keyword, return a list of bool signifying which block has what keyword in it
searchBlogContentsForKeyword :: Text -> Blog -> PList Bool
searchBlogContentsForKeyword keyword blogs = 
   case blogs of 
      End -> Nil
      Layout1 header id author date content tags rst  -> case content of 
         Content block -> let exists    = isKeywordPresentInBlock keyword block
                              existsRst = searchBlogContentsForKeyword keyword rst
                           in Cons exists existsRst
      Layout2 content tags rst header id author date  -> case content of 
         Content block -> let exists    = isKeywordPresentInBlock keyword block
                              existsRst = searchBlogContentsForKeyword keyword rst
                           in Cons exists existsRst
      Layout3 tags rst content header id author date  -> case content of 
         Content block -> let exists    = isKeywordPresentInBlock keyword block
                              existsRst = searchBlogContentsForKeyword keyword rst
                           in Cons exists existsRst
      Layout4 tags content rst header id author date  -> case content of 
         Content block -> let exists    = isKeywordPresentInBlock keyword block
                              existsRst = searchBlogContentsForKeyword keyword rst
                           in Cons exists existsRst
      

-- Filter the Blogs based on if some keyword is present in the Contents field of the Blogs or not 
filterBlogsBasedOnKeywordInContent :: Text -> Blog -> Blog 
filterBlogsBasedOnKeywordInContent keyword blogs = 
   case blogs of 
      End -> End 
      Layout1 header id author date content tags rst -> case content of 
         Content block -> let exists = isKeywordPresentInBlock keyword block 
                              rst'   = filterBlogsBasedOnKeywordInContent keyword rst
                              in if (exists) then Layout1 header id author date (copyPacked content) tags rst'
                                 else rst'
      Layout2 content tags rst header id author date -> case content of 
         Content block -> let exists = isKeywordPresentInBlock keyword block 
                              rst'   = filterBlogsBasedOnKeywordInContent keyword rst
                              in if (exists) then Layout2 (copyPacked content) tags rst' header id author date
                                 else rst'
      Layout3 tags rst content header id author date -> case content of 
         Content block -> let exists = isKeywordPresentInBlock keyword block 
                              rst'   = filterBlogsBasedOnKeywordInContent keyword rst
                              in if (exists) then Layout3 tags rst' (copyPacked content) header id author date
                                 else rst'
      Layout4 tags content rst header id author date -> case content of 
         Content block -> let exists = isKeywordPresentInBlock keyword block 
                              rst'   = filterBlogsBasedOnKeywordInContent keyword rst
                              in if (exists) then Layout4 tags (copyPacked content) rst' header id author date
                                 else rst'

-- Emphasize a particular keyword in a Block type
emphasizeKeywordInBlock :: Text -> Block -> Block
emphasizeKeywordInBlock keyword contentBlock = 
   case contentBlock of 
      Plain list_inline      -> Plain (emphasizeInlineListForKeyword keyword list_inline)
      --Para  list_inline      -> Para  (emphasizeInlineListForKeyword keyword list_inline)
      --BlockQuote list_block  -> BlockQuote (emphasizeKeywordInBlockList keyword list_block)
      --HorizontalRule         -> HorizontalRule
      Null                   -> Null

-- Emphasize a particular keyword in an Inline data type
emphasizeKeywordInline :: Text -> Inline -> Inline 
emphasizeKeywordInline keyword inline = 
   case inline of 
      Str text           -> let isSame = compareWord keyword text 
                                in if (isSame) then let
                                       newlist :: PList Inline 
                                       newlist = (Cons (copyPacked inline)) Nil                         -- ---> Here we had to use a call to copyPacked in order to copy over the inline to a new region, otherwise segfaults. 
                                    in (Emph newlist)
                                   else inline
      Emph list_inline        -> Emph (emphasizeInlineListForKeyword keyword list_inline)
      --Underline list_inline   -> Underline (emphasizeInlineListForKeyword keyword list_inline)
      --Strong list_inline      -> Strong (emphasizeInlineListForKeyword keyword list_inline)
      --Strikeout list_inline   -> Strikeout (emphasizeInlineListForKeyword keyword list_inline)
      --Superscript list_inline -> Superscript (emphasizeInlineListForKeyword keyword list_inline)
      --Subscript list_inline   -> Subscript (emphasizeInlineListForKeyword keyword list_inline) 
      --SmallCaps list_inline   -> SmallCaps (emphasizeInlineListForKeyword keyword list_inline) 
      Space                   -> Space
      --SoftBreak               -> SoftBreak 
      --LineBreak               -> LineBreak 
      --Note list_block         -> Note (emphasizeKeywordInBlockList keyword list_block) 

-- Emphasize a particular keyword in an Inline list
emphasizeInlineListForKeyword :: Text -> PList Inline -> PList Inline
emphasizeInlineListForKeyword keyword inline_list = 
   case inline_list of 
      Nil                -> Nil 
      Cons inline rst    -> let 
                             newinline = emphasizeKeywordInline keyword inline
                             rst'      = emphasizeInlineListForKeyword keyword rst  
                             in Cons newinline rst'

-- Emphasize a particular keyword in a block list
emphasizeKeywordInBlockList :: Text -> PList Block -> PList Block 
emphasizeKeywordInBlockList keyword block_list = 
   case block_list of 
      Nil             -> Nil
      Cons block rst  -> let
                           newBlock = emphasizeKeywordInBlock keyword block 
                           rst'     = emphasizeKeywordInBlockList keyword rst 
                           in Cons newBlock rst'

-- Emphasize a particular keyword in the blogs, return new Blogs
emphasizeBlogContentsForKeyword :: Text -> Blog -> Blog
emphasizeBlogContentsForKeyword keyword blogs = 
   case blogs of 
      End -> End
      Layout1 header id author date content tags rst  -> case content of 
         Content block -> let new_content   = Content (emphasizeKeywordInBlock keyword block)
                              existsRst     = emphasizeBlogContentsForKeyword keyword rst
                           in Layout1 header id author date (copyPacked new_content ) tags existsRst
      Layout2 content tags rst header id author date  -> case content of 
         Content block -> let new_content   = Content (emphasizeKeywordInBlock keyword block)
                              existsRst     = emphasizeBlogContentsForKeyword keyword rst
                           in Layout2 (copyPacked new_content) tags existsRst header id author date
      Layout3 tags rst content header id author date  -> case content of 
         Content block -> let new_content   = Content (emphasizeKeywordInBlock keyword block)
                              existsRst     = emphasizeBlogContentsForKeyword keyword rst
                           in Layout3 tags existsRst (copyPacked new_content) header id author date
      Layout4 tags content rst header id author date  -> case content of 
         Content block -> let new_content   = Content (emphasizeKeywordInBlock keyword block)
                              existsRst     = emphasizeBlogContentsForKeyword keyword rst
                           in Layout4 tags (copyPacked new_content) existsRst header id author date
                           
-- search blog tags 
searchBlogTags :: Text -> BlogTags -> Bool 
searchBlogTags keyword tags = case tags of 
   TagList list -> searchTagList keyword list

-- emphasize blog content, if present is True
emphasizeBlogContent :: Text -> BlogContent -> Bool -> BlogContent
emphasizeBlogContent keyword oldContent present = case oldContent of 
                                                         Content block -> if (present)
                                                                          then Content (emphasizeKeywordInBlock keyword block)
                                                                          else Content block
                                              

-- Look for a keyword in the tag-list and emphasize a particular keyword based on that. 
emphBasedOnTagList :: Text -> Blog -> Blog 
emphBasedOnTagList keyword blogs = 
   case blogs of 
      End -> End 
      Layout1 header id author date content tags rst -> let present    = searchBlogTags keyword tags
                                                            newContent = emphasizeBlogContent keyword content present
                                                            newRst     = emphBasedOnTagList keyword rst
                                                          in Layout1 header id author date (copyPacked newContent) (copyPacked tags) (copyPacked newRst)
      Layout2 content tags rst header id author date -> let present    = searchBlogTags keyword tags 
                                                            newContent = emphasizeBlogContent keyword content present
                                                            newRst     = emphBasedOnTagList keyword rst
                                                          in Layout2 (newContent) (copyPacked tags) (copyPacked newRst) header id author date                                                          
      Layout3 tags rst content header id author date -> let present = searchBlogTags keyword tags 
                                                            newContent = emphasizeBlogContent keyword content present
                                                            newRst = emphBasedOnTagList keyword rst                                        
                                                          in Layout3 tags (newRst) (newContent) header id author date
      Layout4 tags content rst header id author date -> let present = searchBlogTags keyword tags 
                                                            newContent = emphasizeBlogContent keyword content present
                                                            newRst = emphBasedOnTagList keyword rst
                                                          in Layout4 tags (newContent) (newRst) header id author date
      Layout5 rst tags content header id author date -> let present = searchBlogTags keyword tags 
                                                            newContent = emphasizeBlogContent keyword content present
                                                            newRst = emphBasedOnTagList keyword rst 
                                                          in Layout5 newRst (copyPacked tags) (copyPacked newContent) header id author date    
      Layout6 header id author date content rst tags -> let present = searchBlogTags keyword tags
                                                            newContent = emphasizeBlogContent keyword content present 
                                                            newRst = emphBasedOnTagList keyword rst
                                                          in Layout6 header id author date (newContent) (newRst) tags
      Layout7 rst content header id author date tags -> let present = searchBlogTags keyword tags 
                                                            newContent = emphasizeBlogContent keyword content present
                                                            newRst = emphBasedOnTagList keyword rst
                                                          in Layout7 (newRst) (copyPacked newContent) header id author date tags
      Layout8 content rst id author date header tags -> let present = searchBlogTags keyword tags
                                                            newContent = emphasizeBlogContent keyword content present 
                                                            newRst     = emphBasedOnTagList keyword rst 
                                                          in Layout8 (newContent) (newRst) id author date header tags

searchBlogContent :: Text -> BlogContent -> Bool 
searchBlogContent keyword content = case content of 
      Content block -> (isKeywordPresentInBlock keyword block)
                    

fileToContent :: Vector Char -> Vector Char -> PList Inline -> Int -> Int -> Block
fileToContent file word plist_inline index max_len = 
      if index >= max_len then (Plain plist_inline)                                                                     
                           else let 
                                 character :: Char
                                 character    = nth file index
                                 isSpace      = if ( character *==* (head " ") ) then True else False
                                 char_vec     = (singleton character)
                                 plist_space :: PList Inline
                                 plist_space = (Cons (Space) plist_inline) 
                                in if (isSpace) then (fileToContent file (singleton (nth file (index+1))) (Cons (Str word) plist_inline) (index+2) max_len)  
                                                else (fileToContent file (append word char_vec) (plist_inline) (index+1) max_len)


mkContentFromText :: Int -> BlogContent 
mkContentFromText val = 
   if      (val == 0) then 
                               let f :: Vector Char 
                                   f       = readArrayFile (Just ("blog1/blog1Out.txt",  502)) 
                                   block   = fileToContent f  (singleton (nth f  0)) Nil 1 (vlength f) 
                                in mkBlogContent block
   else if (val == 1) then 
                               let f :: Vector Char 
                                   f       = readArrayFile (Just ("blog2/blog2Out.txt",  400)) 
                                   block   = fileToContent f  (singleton (nth f  0)) Nil 1 (vlength f) 
                                in mkBlogContent block
   else if (val == 2) then 
                                 let f :: Vector Char 
                                     f       = readArrayFile (Just ("blog3/blog3Out.txt",  340)) 
                                     block   = fileToContent f  (singleton (nth f  0)) Nil 1 (vlength f) 
                                in mkBlogContent block
   else if (val == 3) then 
                               let f :: Vector Char 
                                   f       = readArrayFile (Just ("blog4/blog4Out.txt",  347)) 
                                   block   = fileToContent f  (singleton (nth f  0)) Nil 1 (vlength f) 
                                in mkBlogContent block
   else if (val == 4) then 
                               let f :: Vector Char 
                                   f       = readArrayFile (Just ("blog5/blog5Out.txt",  452)) 
                                   block   = fileToContent f  (singleton (nth f  0)) Nil 1 (vlength f) 
                                in mkBlogContent block     
   else if (val == 5) then 
                               let f :: Vector Char 
                                   f       = readArrayFile (Just ("blog6/blog6Out.txt",  260)) 
                                   block   = fileToContent f  (singleton (nth f  0)) Nil 1 (vlength f) 
                                 in mkBlogContent block  
   else if (val == 6) then 
                               let f :: Vector Char 
                                   f       = readArrayFile (Just ("blog7/blog7Out.txt",  395)) 
                                   block   = fileToContent f  (singleton (nth f  0)) Nil 1 (vlength f) 
                                in mkBlogContent block
   else if (val == 7) then 
                                let f :: Vector Char 
                                    f       = readArrayFile (Just ("blog7/blog7Out.txt",  404)) 
                                    block   = fileToContent f  (singleton (nth f  0)) Nil 1 (vlength f) 
                                 in mkBlogContent block     
   else if (val == 8) then 
                                 let f :: Vector Char 
                                     f       = readArrayFile (Just ("blog8/blog8Out.txt",  278)) 
                                     block   = fileToContent f  (singleton (nth f  0)) Nil 1 (vlength f) 
                                  in mkBlogContent block
   else
         let f :: Vector Char 
             f       = readArrayFile (Just ("blog9/blog9Out.txt",  299)) 
             block   = fileToContent f  (singleton (nth f  0)) Nil 1 (vlength f) 
          in mkBlogContent block

                                             

-- main function 
--gibbon_main = 
---   let blogs1  = mkBlogs_layout1 20000 0 1000       -- mkBlogs_layout1 length start_id tag_length
--       blogs2  = mkBlogs_layout2 20000 0 1000
--       blogs3  = mkBlogs_layout3 20000 0 1000
--       blogs4  = mkBlogs_layout4 20000 0 1000
--       blogs5  = mkBlogs_layout5 20000 0 1000
--       blogs6  = mkBlogs_layout6 20000 0 1000
--       blogs7  = mkBlogs_layout7 20000 0 1000
--       blogs8  = mkBlogs_layout8 20000 0 1000
--       keyword = (getRandomString 2)             -- some random keyword
       --new_blogs1 = iterate (filterBlogsBasedOnKeywordInTagList keyword blogs1)
       --new_blogs2 = iterate (filterBlogsBasedOnKeywordInTagList keyword blogs2)
       --new_blogs3 = iterate (filterBlogsBasedOnKeywordInTagList keyword blogs3)
       --new_blogs4 = iterate (filterBlogsBasedOnKeywordInTagList keyword blogs4)
       --new_blogs5 = iterate (filterBlogsBasedOnKeywordInTagList keyword blogs5)
       --new_blogs6 = iterate (filterBlogsBasedOnKeywordInTagList keyword blogs6)
       --new_blogs7 = iterate (filterBlogsBasedOnKeywordInTagList keyword blogs7)
       --a          = emphBasedOnTagList keyword blogs1
       --_          = printPacked a
       -- new_blogs1 = iterate (emphBasedOnTagList keyword blogs1) 
       -- new_blogs2 = iterate (emphBasedOnTagList keyword blogs2) 
       -- new_blogs3 = iterate (emphBasedOnTagList keyword blogs3) 
       -- new_blogs4 = iterate (emphBasedOnTagList keyword blogs4) 
       -- new_blogs5 = iterate (emphBasedOnTagList keyword blogs5) 
       -- new_blogs6 = iterate (emphBasedOnTagList keyword blogs6) 
       -- new_blogs7 = iterate (emphBasedOnTagList keyword blogs7)
       -- new_blogs8 = iterate (emphBasedOnTagList keyword blogs8)
       --new_blogs1 = iterate (emphKeywordInContent keyword blogs1)
       --new_blogs2 = iterate (emphKeywordInContent keyword blogs2)
       --new_blogs3 = iterate (emphKeywordInContent keyword blogs3) 
       --new_blogs4 = iterate (emphKeywordInContent keyword blogs4) 
       --new_blogs5 = iterate (emphKeywordInContent keyword blogs5) 
       --new_blogs6 = iterate (emphKeywordInContent keyword blogs6) 
       --new_blogs7 = iterate (emphKeywordInContent keyword blogs7)
       --new_blogs8 = iterate (emphKeywordInContent keyword blogs8)
       --_ = printPacked new_blogs8
       --_          = printsym (quote "NEWLINE")
       --_          = printsym (quote "NEWLINE") 
       --_          = printPacked new_blogs1
       --_          = printsym (quote "NEWLINE")
       --_          = printsym (quote "NEWLINE") 
       --_          = printPacked new_blogs2
       --_          = printsym (quote "NEWLINE")
       --_          = printsym (quote "NEWLINE") 
       --_          = printPacked new_blogs3
       --_          = printsym (quote "NEWLINE")
       --_          = printsym (quote "NEWLINE") 
       --_          = printPacked new_blogs4
       --_          = printsym (quote "NEWLINE") 
       --_          = printsym (quote "NEWLINE")
       --_          = printPacked new_blogs5
       --_          = printsym (quote "NEWLINE") 
       --_          = printsym (quote "NEWLINE")
       --_          = printPacked new_blogs6
       --_          = printsym (quote "NEWLINE")
       --_          = printsym (quote "NEWLINE") 
       --_          = printPacked new_blogs7
       --_          = printsym (quote "NEWLINE") 
       --_          = printsym (quote "NEWLINE")
       --_          = printPacked new_blogs8
       --_          = printsym (quote "NEWLINE") 
       --_          = printsym (quote "NEWLINE")
       --_          = printPacked new_blogs1
       --_          = printPacked new_blogs2
       --_          = printPacked new_blogs3
       --_          = printPacked new_blogs4
       --_          = printPacked new_blogs5
      --  some_text    = (getRandomString (mod rand 9))
      --  _            = printsym (quote "NEWLINE")
      --  _            = printsym (quote "NEWLINE")
      --  _            = printPacked blogs
      --  _            = printsym (quote "NEWLINE")
      --  _            = printsym (quote "NEWLINE")
      --  exists       = searchBlogContentsForKeyword keyword blogs
      --  _            = printsym (quote "Does keyword exist in blogs: ")
      --  _            = printPacked exists 
      --  _            = printsym (quote "NEWLINE")
      --  _            = printsym (quote "NEWLINE")
      --  newBlogsTags = filterBlogsBasedOnKeywordInTagList keyword blogs
      --  _            = printsym (quote "Print the new blogs after a searching for keywords in the TagList")
      --  _            = printsym (quote "NEWLINE")
      --  _            = printPacked newBlogsTags
      --  _            = printsym (quote "NEWLINE")
      --  _            = printsym (quote "NEWLINE")
      --  newBlogsCont = filterBlogsBasedOnKeywordInContent keyword blogs
      --  _            = printsym (quote "Print the new blogs after searching for keywords in Content")
      --  _            = printsym (quote "NEWLINE")
      --  _            = printPacked newBlogsTags
      --  _            = printsym (quote "NEWLINE")
      --  _            = printsym (quote "NEWLINE")
      --  word1        = "Same"
      --  word2        = "Same"
      --  similar      = compareWord word1 word2
      --  _            = printsym (quote "Print results after comparing two words: ")
      --  _            = printbool similar
      --  _            = printsym (quote "NEWLINE")
      --  _            = printsym (quote "NEWLINE")
      --  _            = printsym (quote "Print blogs after emphasizing a particular keyword.")
      --  newBlogsEmph = emphasizeBlogContentsForKeyword keyword blogs
      --  _            = printPacked newBlogsEmph
      --  _            = printsym (quote "NEWLINE")
      --  _            = printsym (quote "NEWLINE")
   --in ()
       

-- gibbon --packed --no-gc --toC Main.hs; gcc -g Main.c -o main

-- Passes written so far

-- 1.) Search for a keyword in Content of Blogs 
-- 2.) Search for a keyword in TagList of Blogs 
-- 3.) Filter blogs based on a particular keyword
-- 4.) Filter blogs based on a particular keyword in the TagList
-- 5.) emphasize a particular keyword in the Content of Blogs
