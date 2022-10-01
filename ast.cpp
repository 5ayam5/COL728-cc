#include "ast.hpp"

vector<ExternalDeclaration *> declarations;

void CompoundStatement::add(BlockItem *blockItem) { blockItemList.push_back(blockItem); }
