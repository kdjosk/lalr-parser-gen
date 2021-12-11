use crate::lalr_parsing_tables::{LALRParsingTables};
struct LRParser {
    parsing_tables: LALRParsingTables
}
impl LRParser {
    pub fn new(parsing_tables: LALRParsingTables) -> LRParser {
        LRParser{
            parsing_tables
        }
    }             
}