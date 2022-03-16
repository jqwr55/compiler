/*
u32 WalkIATreeAtCallSiteForObservation(Compiler* compiler, StaticBuffer<IntegerRange> offset, u32 baseAddressLoadedinCall, IndirectAddress root, IndirectAddress tree, StaticBuffer<u32> loads, RangeWithBase* result) {
    
    byte* const baseMem = compiler->mem;
    u32 ret = 0;

    for(u32 k = 0; k < tree.indirects.edgeCount; k++) {

        IndirectAddress* indirect = (IndirectAddress*)(baseMem + tree.indirects.edges + k * sizeof(IndirectAddress));
        IntegerRange* indirectAddressRange = (IntegerRange*)(baseMem + indirect->storedRanges.edges);

        StaticBuffer<IntegerRange> loadOffset;
        loadOffset.memory = (IntegerRange*)((byte*)result+ret);
        loadOffset.size = GetMemoryRanges(compiler, indirect->storedAddress, 0, loadOffset.memory);
        IntegerRange localMem[loadOffset.size];
        for(u32 i = 0; i < loadOffset.size; i++) {
            localMem[i].max.i = i64(0);
            localMem[i].min.i = i64(0);
            localMem[i].tag = RANGE_SIGNED;
        }
        AddIntegerRanges(compiler, localMem, loadOffset.memory, loadOffset.size, loadOffset.size, TOKEN_MINUS);
        loadOffset.memory = localMem;

        for(u32 i = 0; i < loads.size; i++) {
            SSADefinition* load = (SSADefinition*)(baseMem + loads[i]);
            u32* visited = (u32*)((byte*)result+ret);
            visited[0] = 0;
            u32 loadBaseAdd = GetBaseAddressFromPointer(compiler, load->operand0ValueDef, visited);
            visited[0] = 0;
            bool sameBase = EQSSA(compiler, loadBaseAdd, baseAddressLoadedinCall, visited);
            
            if(sameBase) {

                IntegerRange* observedRange = (IntegerRange*)((byte*)result+ret);
                u32 observedRangeCount = GetMemoryRanges(compiler, load->operand0ValueDef, 8, observedRange);
                IntegerRange observedRangeBuffer[observedRangeCount];
                AddIntegerRanges(compiler, observedRange, offset.memory, observedRangeCount, offset.size, TOKEN_PLUS);
                memcpy(observedRangeBuffer, observedRange, observedRangeCount*sizeof(IntegerRange));

                bool observed = MemoryClobbered(observedRange, observedRangeCount, indirectAddressRange, indirect->storedRanges.edgeCount);
                if(observed) {

                    RangeWithBase* res = (RangeWithBase*)(((byte*)result+ret));
                    res->baseAddress = loadBaseAdd;
                    res->range;
                    memcpy(res->range, observedRangeBuffer, observedRangeCount * sizeof(IntegerRange));
                    res->rangeCount = observedRangeCount;
                    ret += sizeof(RangeWithBase) + observedRangeCount * sizeof(IntegerRange);

                    ret += WalkIATreeAtCallSiteForObservation(compiler, loadOffset, loads[i], tree, *indirect, loads, (RangeWithBase*)((byte*)result+ret));
                }
            }
        }
    }

    return ret;
}

u32 WalkIndirectAddressTreeAtCallSite(Compiler* compiler, StaticBuffer<IntegerRange> offset, u32 baseAddressLoadedinCall, IndirectAddress root, IndirectAddress tree, StaticBuffer<u32> stores, StaticBuffer<u32> loads, RangeWithBase* result) {

    byte* const baseMem = compiler->mem;
    u32 ret = 0;
    for(u32 i = 0; i < stores.size; i++) {

        SSADefinition* store = (SSADefinition*)(baseMem + stores[i]);
        u32* visited = (u32*)((byte*)result + ret);
        visited[0] = 0;
        u32 storeBaseAdd = GetBaseAddressFromPointer(compiler, store->operand0ValueDef, visited);

        visited[0] = 0;
        bool clobbered = EQSSA(compiler, storeBaseAdd, baseAddressLoadedinCall, visited);
        if(clobbered) {

            RangeWithBase* res = (RangeWithBase*)((byte*)result + ret);          
            res->baseAddress = tree.baseAaddress;
            u32 storeSize = GetTypeSize(baseMem, baseMem, store->type) - 1;
            res->rangeCount = GetMemoryRanges(compiler, store->operand0ValueDef, storeSize, res->range);
            AddIntegerRanges(compiler, res->range, offset.memory, res->rangeCount, offset.size, TOKEN_PLUS);
            
            ret += sizeof(RangeWithBase) + sizeof(IntegerRange) * res->rangeCount;
        }
    }

    for(u32 k = 0; k < tree.indirects.edgeCount; k++) {

        IndirectAddress* indirect = (IndirectAddress*)(baseMem + tree.indirects.edges + k * sizeof(IndirectAddress));
        IntegerRange* indirectAddressRange = (IntegerRange*)(baseMem + indirect->storedRanges.edges);

        StaticBuffer<IntegerRange> loadOffset;
        loadOffset.memory = (IntegerRange*)((byte*)result+ret);
        loadOffset.size = GetMemoryRanges(compiler, indirect->storedAddress, 0, loadOffset.memory);
        IntegerRange localMem[loadOffset.size];
        for(u32 i = 0; i < loadOffset.size; i++) {
            localMem[i].max.i = i64(0);
            localMem[i].min.i = i64(0);
            localMem[i].tag = RANGE_SIGNED;
        }
        AddIntegerRanges(compiler, localMem, loadOffset.memory, loadOffset.size, loadOffset.size, TOKEN_MINUS);
        loadOffset.memory = localMem;

        for(u32 i = 0; i < loads.size; i++) {
            SSADefinition* load = (SSADefinition*)(baseMem + loads[i]);
            u32* visited = (u32*)((byte*)result+ret);
            visited[0] = 0;
            u32 loadBaseAdd = GetBaseAddressFromPointer(compiler, load->operand0ValueDef, visited);
            visited[0] = 0;
            bool sameBase = EQSSA(compiler, loadBaseAdd, baseAddressLoadedinCall, visited);
            
            if(sameBase) {

                IntegerRange* observedRange = (IntegerRange*)((byte*)result+ret);
                u32 observedRangeCount = GetMemoryRanges(compiler, load->operand0ValueDef, 8, observedRange);
                AddIntegerRanges(compiler, observedRange, offset.memory, observedRangeCount, offset.size, TOKEN_PLUS);
                bool observed = MemoryClobbered(observedRange, observedRangeCount, indirectAddressRange, indirect->storedRanges.edgeCount);
                if(observed) {
                    ret += WalkIndirectAddressTreeAtCallSite(compiler, loadOffset, loads[i], tree, *indirect, stores, loads, (RangeWithBase*)((byte*)result+ret));
                }
            }
        }
    }

    return ret;
}


u32 FindStoresFromBasePtr(Compiler* compiler, SSABasicBlock* block, SSADefinition* first, u32 basePtr, u32* result) {

    u32 ret = 0;
    byte* const baseMem = compiler->mem;
    SSADefinition* it = first;
    while(it) {

        if(it->opr == EXPRESSION_MEMORY_STORE) {
            result[ret+1] = 0;
            u32 base = GetBaseAddressFromPointer(compiler, it->operand0ValueDef, result+ret+1);
            if(base == basePtr) {
                result[ret++] = (u64)it - (u64)baseMem;
            }
        }

        it = GetNextDef(baseMem, it->prevDef);
    }

    for(u32 i = 0; i < block->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
        SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
        ret += FindStoresFromBasePtr(compiler, predBlock, predBlock->lastDef, basePtr, result+ret);
    }
    return ret;
}



u32 GetIndirectAddressStores(Compiler* compiler, SSAMemoryDef* currentMemory, u32* result) {

    byte* const baseMem = compiler->mem;

    SSABasicBlock* block;
    SSADefinition* first;
    if(currentMemory->type == MEMORY_DEF) {
        SSADefinition* store = (SSADefinition*)(baseMem + currentMemory->ssaDef);
        first = store;
        block = (SSABasicBlock*)(baseMem + store->block);
    }
    else if(currentMemory->type == MEMORY_PHI) {
        block = (SSABasicBlock*)(baseMem + currentMemory->ssaDef);
        first = block->firstDef;
    }
    else {
        return 0;
    }

    return FindStoresFromBasePtr(compiler, block, first, currentMemory->basePtrDef, result);
}


u32 FindFirstMemoryFromBasePtr(Compiler* compiler, u32 baseAddress, SSABasicBlock* block, SSADefinition* first, u32* mem) {

    u32 ret;
    byte* const baseMem = compiler->mem;
    SSADefinition* it = first;
    while(it) {

        if(it->opr == EXPRESSION_MEMORY_STORE) {
            mem[0] = 0;
            u32 base = GetBaseAddressFromPointer(compiler, it->operand0ValueDef, mem);
            if(base == baseAddress) {
                return it->extraPtr;
            }
        }

        it = GetNextDef(baseMem, it->prevDef);
    }

    u32 name = GetBasePtrIndexFromDef(compiler, (SSADefinition*)(baseMem + baseAddress));
    u32 recentBlock;
    for(u32 i = 0; i < block->memoryPtrs.size; i++) {
        if(block->memoryPtrs[i].key == name) {
            recentBlock = block->memoryPtrs[i].value;
            break;
        }
    }

    for(u32 i = 0; i < block->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
        SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
        ret += FindFirstMemoryFromBasePtr(compiler, baseAddress, predBlock, predBlock->lastDef, mem);
    }
}

SSAMemoryDef* GetNextMem(Compiler* compiler, u32 ptr) {
    return ptr == 0 ? nullptr : (SSAMemoryDef *)(compiler->mem + ptr);
}

SSAMemoryDef* FindFirstMemoryWithBasePtr(Compiler* compiler, SSABasicBlock* block, SSAMemoryDef* first, u32 baseAddress) {

    byte* const baseMem = compiler->mem;
    SSAMemoryDef* it = first;
    while(it) {

        if(it->basePtrDef == baseAddress) {
            return it;
        }

        it = GetNextMem(compiler, it->prev);
    }

    for(u32 i = 0; i < block->predecessors.edgeCount; i++) {
        u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
        SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
        auto ret = FindFirstMemoryWithBasePtr(compiler, predBlock, predBlock->lastMem, baseAddress);
        if(ret) return ret;
    }

    return nullptr;
}

IndirectAddress BuildIndirectAddressTree(Compiler* compiler, SSAMemoryDef* origin, SSAMemoryDef* currentMemory, byte** result) {

    byte* const baseMem = compiler->mem;
    IndirectAddress root{};
    root.baseAaddress = currentMemory->basePtrDef;
    u32 storeCount = GetIndirectAddressStores(compiler, currentMemory, *((u32**)result));

    u32* localRes = *((u32**)result);
    *result += storeCount * sizeof(u32) + storeCount * sizeof(IndirectAddress);

    root.indirects.edgeCount = storeCount;
    root.indirects.edges = (byte*)(localRes+storeCount) - baseMem;
    for(u32 i = 0; i < storeCount; i++) {
        SSADefinition* indirectStore = (SSADefinition*)(baseMem + localRes[i]);
        localRes[i] = indirectStore->operand1ValueDef;
        ((u32*)(*result))[0] = 0;
        u32 baseAddressPtr = GetBaseAddressFromPointer(compiler, localRes[i], (u32*)(*result));

        SSAMemoryDef* indirectStoreDef = (SSAMemoryDef*)(baseMem + indirectStore->extraPtr);

        ((IndirectAddress*)(localRes+storeCount))[i] = {};
        auto indirect = FindFirstMemoryWithBasePtr(compiler, GetBlockFromMemory(compiler, origin), origin, baseAddressPtr);
        if(!indirect) {
            ((IndirectAddress*)(localRes+storeCount))[i].baseAaddress = baseAddressPtr;
        }
        else {
            ((IndirectAddress*)(localRes+storeCount))[i] = BuildIndirectAddressTree(compiler, origin, indirect, result);
        }    
        ((IndirectAddress*)(localRes+storeCount))[i].storedRanges.edges = indirectStoreDef->addressRanges.edges;
        ((IndirectAddress*)(localRes+storeCount))[i].storedRanges.edgeCount = indirectStoreDef->addressRanges.edgeCount;
        ((IndirectAddress*)(localRes+storeCount))[i].storedAddress = indirectStore->operand1ValueDef;
    }

    return root;
}
void PrintIndirectAddressTree(Compiler* compiler, IndirectAddress root, u32 depth) {

    byte* const baseMem = compiler->mem;
    global_print("%i", root.baseAaddress);
    for(u32 i = 0; i < root.storedRanges.edgeCount; i++) {
        PrintIntRange(Mem<IntegerRange>(baseMem + root.storedRanges.edges + i * sizeof(IntegerRange)));
    }
    global_print("%\n");
    for(u32 i = 0; i < root.indirects.edgeCount; i++) {
        IndirectAddress* indirect = (IndirectAddress*)(baseMem + root.indirects.edges + i * sizeof(IndirectAddress));
        for(u32 i = 0; i < depth; i++) global_print("%c", '\t');
        PrintIndirectAddressTree(compiler, *indirect, depth+1);
    }
}
*/


/*
bool BasePtrDerivedFrom(Compiler* compiler, u32 basePtr, u32 from, u32* mem, u32* visited) {

    byte* const baseMem = compiler->mem;
    SSADefinition* base = (SSADefinition*)(baseMem + basePtr);   
    if(base->opr != EXPRESSION_MEMORY_LOAD) return false;

    visited[0] = 0;
    u32 loadBase = GetBaseAddressFromPointer(compiler, base->operand0ValueDef, mem, visited);
    if(loadBase == from) return true;
    return BasePtrDerivedFrom(compiler, loadBase, from, mem, visited);
}

u32 GetMemoryFromBasePtrInBlock(Compiler* compiler, SSABasicBlock* block, SSADefinition* base, byte* incompletePhibuffer, u32* visited) {

    u32 ret = ~u32(0);
    byte *const baseMem = compiler->mem;
    u32 name = GetBasePtrIndexFromDef(compiler, base);
    if(name == ~u32(0)) {
        return MakeUnkownMemory(compiler, (byte*)base - baseMem, visited+visited[0]+1);
    }
    
    for(u32 i = 0; i < block->memoryPtrs.size; i++) {
        if(block->memoryPtrs[i].key == name) {
            return block->memoryPtrs[i].value;
        }
    }

    if(block->predecessors.edgeCount == 1) {
        u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges);
        SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
        ret = GetMemoryFromBasePtrInBlock(compiler, predBlock, base, incompletePhibuffer, visited);
    }
    else if(block->predecessors.edgeCount != 0) {

        ret = compiler->miscAllocatorSSA;
        SSAMemoryDef* memoryPhi = AddMemoryPhi(compiler, block);
        memoryPhi->basePtrDef = (u64)base - (u64)baseMem;
        WriteMemoryToBlock(compiler, block, ret, name);

        u32 preMemories[block->predecessors.edgeCount];
        for(u32 i = 0; i < block->predecessors.edgeCount; i++) {
            u32 predBlockPtr = Mem<u32>(baseMem + block->predecessors.edges + i * sizeof(u32));
            SSABasicBlock* predBlock = (SSABasicBlock*)(baseMem + predBlockPtr);
            preMemories[i] = GetMemoryFromBasePtrInBlock(compiler, predBlock, base, incompletePhibuffer, visited);
        }
        
        for(u32 i = 0; i < block->predecessors.edgeCount; i++) {
            InsertMemoryEdge(compiler, preMemories[i], ret);
        }
    }
    else {
        u32 unInitMemoryPtr = compiler->miscAllocatorSSA;
        SSAMemoryDef* unInitMemory = (SSAMemoryDef*)AllocateSSA<SSAMemoryDef>(compiler);
        *unInitMemory = {};
        unInitMemory->type = MEMORY_UNKOWN;
        unInitMemory->basePtrDef = (u64)base - (u64)baseMem;
        unInitMemory->name = compiler->uniqueMemoryName++;
        PushFrontMem(compiler, block, unInitMemoryPtr);

        SSABasicBlock* baseBlock = (SSABasicBlock*)(baseMem + base->block);
        baseBlock->memoryPtrs.PushBack({GetBasePtrIndexFromDef(compiler, base), unInitMemoryPtr});
        ret = unInitMemoryPtr;
    }
    WriteMemoryToBlock(compiler, block, ret, name);
    return ret;
}

void ClobberWorldAtFunctionCall(Compiler *compiler, SSABasicBlock* block, u32 call, byte* incompletePhiBuffer, u32* visitMem) {

    byte* const baseMem = compiler->mem;
    SSADefinition* callDef = (SSADefinition*)(baseMem + call);
    SSAFunction* fn = (SSAFunction*)(baseMem + compiler->currentFunction);

    callDef->extraPtr = compiler->miscAllocatorSSA;
    SSACallInfo* extra = (SSACallInfo*)AllocateSSA(compiler, sizeof(SSACallInfo) + fn->graph.vertexCount * sizeof(u32) + fn->graph.vertexCount * (sizeof(RangeWithBase) + sizeof(IntegerRange)));
    extra->memoryDefCount = fn->graph.vertexCount;
    for(u32 i = 0; i < fn->graph.vertexCount; i++) {
        
        auto lower = (SSADefinition*)pool_allocate(&compiler->ssaDefPool);
        lower->opr = NEGATIVE_INF;
        auto upper = (SSADefinition*)pool_allocate(&compiler->ssaDefPool);
        lower->opr = POSITIVE_INF;
        SymbolicRange range{(byte*)lower-baseMem, (byte*)upper-baseMem};

        SSADefinition* baseAddressDef = (SSADefinition*)(baseMem + fn->graph.vertices[i].basePtr);
        visitMem[0] = 0;
        auto memory = MakeMemory(compiler, block, call, fn->graph.vertices[i].basePtr, &range, 1);
        auto prevMem = GetMemoryFromBasePtrInBlock(compiler, block, baseAddressDef, incompletePhiBuffer, visitMem);
        InsertMemoryEdge(compiler, prevMem, memory);
        WriteMemoryToBlock(compiler, block, memory, GetBasePtrIndexFromDef(compiler, baseAddressDef) );
        extra->memoryDefs[i] = memory;
    }
    extra->memoryUseCount = fn->graph.vertexCount;
    u32 off = extra->memoryDefCount * sizeof(u32);
    for(u32 i = 0; i < fn->graph.vertexCount; i++) {

        RangeWithBase* res = (RangeWithBase*)(extra->memoryUses + off);
        res->baseAddress = fn->graph.vertices[i].basePtr;
        res->range[0].tag = RANGE_ALL;
        res->rangeCount = 1;
        off += sizeof(RangeWithBase) + sizeof(IntegerRange);
    }
}

u32 MakeStoreExtraInfo(Compiler *compiler, u32 storeDefPtr, byte* incompletePhiBuffer, u32* visitMem) {

    byte *const baseMem = compiler->mem;
    SSADefinition *storeDef = (SSADefinition *)(baseMem + storeDefPtr);
    SSABasicBlock* storeBlock = (SSABasicBlock*)(baseMem + storeDef->block);

    CpyTypeExpr((byte*)visitMem, {0}, baseMem, storeDef->type);
    u32 storeSize = GetTypeSize(compiler->mem, (byte *)visitMem, {0}) - 1;

    u32* bases = (u32*)GetEndOfIncompletePhiBuffer(incompletePhiBuffer);
    visitMem[0] = 0;
    u32 baseCount = GetBaseAddressFromPointer(compiler, storeDef->operand0ValueDef, bases, visitMem);
    u32 basesLocalCpy[baseCount];
    memcpy(basesLocalCpy, bases, baseCount*sizeof(u32));

    u32 ret = compiler->miscAllocatorSSA;
    SSAStoreInfo* info = (SSAStoreInfo*)AllocateSSA(compiler, sizeof(SSAStoreInfo) + baseCount * sizeof(u32));
    info->memoryDefCount = baseCount;

    for(u32 i = 0; i < baseCount; i++) {

        SSADefinition *baseDef = (SSADefinition *)(baseMem + basesLocalCpy[i]);
        SSADefinition localBaseDefCpy = *baseDef;

        baseDef->opr = EXPRESSION_IMMEDIATE;
        baseDef->operand0ValueDef = compiler->exprAllocator;
        Value *v = AllocateExpr<Value>(compiler);
        *v = MakeImm(TYPE_PRIMARY_INT64, 0);

        auto lower = (SSADefinition*)pool_allocate(&compiler->ssaDefPool);
        lower->opr = NEGATIVE_INF;
        auto upper = (SSADefinition*)pool_allocate(&compiler->ssaDefPool);
        lower->opr = POSITIVE_INF;
        SymbolicRange ranges{(byte*)lower-baseMem, (byte*)upper-baseMem};

        IntegerRange *ranges = (IntegerRange *)visitMem;
        ranges->tag = RANGE_ALL;
        u32 rangesC = 1;

        compiler->exprAllocator -= sizeof(ImmediateExpr);
        *baseDef = localBaseDefCpy;

        for (u32 k = 0; k < rangesC; k++) {
            if (ranges[k].tag == RANGE_SIGNED) {
                ranges[k].max.i = Clamp<i64>(ranges[k].max.i + storeSize, I64_MAX, I64_MIN);
            } else if (ranges[k].tag == RANGE_UNSIGNED) {
                ranges[k].max.u = Clamp<u64>(ranges[k].max.u + storeSize, U64_MAX, U64_MIN);
            }
        }

        visitMem[0] = 0;
        info->memoryDefs[i] = MakeMemory(compiler, storeBlock, storeDefPtr, basesLocalCpy[i], &ranges, 1);
        u32 baseName = GetBasePtrIndexFromDef(compiler, baseDef);
        if(baseName == ~u32(0)) {
            MakeUnkownMemory(compiler, basesLocalCpy[i], visitMem);
        }
        u32 prevMem = SearchMemoryInBlockFromBaseAddressName(compiler, storeBlock, baseName);
        if(prevMem == ~u32(0)) {
            WriteMemoryToBlock(compiler, storeBlock, info->memoryDefs[i], baseName);
            GetMemoryFromBasePtrInBlock(compiler, storeBlock, baseDef, incompletePhiBuffer, visitMem);
        }
        InsertMemoryEdge(compiler, prevMem, info->memoryDefs[i]);
    }

    return ret;
}
u32 GetMemoryRanges(Compiler *compiler, u32 addressPtr, u32 size, byte* result, u32* visitMem) {

    byte *const baseMem = compiler->mem;
    SSADefinition *address = (SSADefinition *)(baseMem + addressPtr);

    visitMem[0] = 0;
    u32 possibleBaseCount = GetBaseAddressFromPointer(compiler, addressPtr, (u32 *)result, visitMem);
    memcpy(visitMem, result, possibleBaseCount*sizeof(u32));
    u32 offset = 0;
    for(u32 i = 0; i < possibleBaseCount; i++) {

        u32 basePtr = visitMem[i];
        SSADefinition *base = (SSADefinition *)(baseMem + basePtr);
        SSADefinition val = *base;

        base->opr = EXPRESSION_IMMEDIATE;
        base->operand0ValueDef = compiler->exprAllocator;
        Value *v = AllocateSSA<Value>(compiler);
        *v = MakeImm(TYPE_PRIMARY_INT64, 0);

        RangeWithBase* rwb = (RangeWithBase*)(result + offset);
        rwb->baseAddress = basePtr;
        rwb->rangeCount = GetDefIntegerRanges(compiler, addressPtr, rwb->range);

        compiler->exprAllocator -= sizeof(ImmediateExpr);
        *base = val;

        for (u32 i = 0; i < rwb->rangeCount; i++) {
            if (rwb->range[i].tag == RANGE_SIGNED) {
                rwb->range[i].max.i = Max(rwb->range[i].max.i, rwb->range[i].max.i + size);
            } else if (rwb->range[i].tag == RANGE_UNSIGNED) {
                rwb->range[i].max.u = Max(rwb->range[i].max.u, rwb->range[i].max.u + size);
            }
        }
        offset += sizeof(RangeWithBase) + rwb->rangeCount * sizeof(u32);
    }
    return possibleBaseCount;
}

u32 GetDefIntegerRanges(Compiler *compiler, u32 defPtr, IntegerRange *mem) {
    byte *const baseMem = compiler->mem;
    SSADefinition *def = (SSADefinition *)(baseMem + defPtr);

    switch (def->opr) {
    case EXPRESSION_LITERAL: 
        {
            LiteralExpr *litr = (LiteralExpr *)(baseMem + def->operand0);
            Value v = GetValueFromLiteral(litr->literal);
            memcpy(&(mem[0].max.i), v.mem, sizeof(u64));
            memcpy(&(mem[0].min.i), v.mem, sizeof(u64));
            mem[0].tag = IsUnsigned((TypeName)v.type) ? RANGE_UNSIGNED : RANGE_SIGNED;
            return 1;
            break;
        }
    case EXPRESSION_IMMEDIATE:
        {
            Value *v = (Value*)(baseMem + def->operand0);
            memcpy(&(mem[0].max.i), v->mem, sizeof(u64));
            memcpy(&(mem[0].min.i), v->mem, sizeof(u64));
            mem[0].tag = IsUnsigned((TypeName)v->type) ? RANGE_UNSIGNED : RANGE_SIGNED;
            return 1;
            break;
        }
    case SSA_PHI_NODE:
        {
            Mem<u32>(mem) = 0;
            bool cycle = DoesPhiFormCycle(baseMem, defPtr, (u32*)mem);

            u32 *ptr = GetPhiOperandPtr(baseMem, def);
            u32 count = GetPhiOperandCount(def);
            if(!cycle) {
                u32 ret = 0;
                for (u32 i = 0; i < count; i++) {
                    ret += GetDefIntegerRanges(compiler, ptr[i], mem + ret);
                }
                return ret;
            }
            else {
                ASSERT(false);
                SSABasicBlock* headerBlock = (SSABasicBlock*)(baseMem + def->block);
                LoopAnalysis loop;
                ComputeLoopTripCount(compiler, &loop, (u32*)mem, (u32*)mem);
            }
        }
    case SSA_FUNCTION:
        {
            mem[0].min.u = 0;
            mem[0].max.u = ~u64(0);
            mem[0].tag = RANGE_UNSIGNED;
            return 1;
            break;
        }
    case SSA_COPY:
        return GetDefIntegerRanges(compiler, def->operand0, mem);
    case SSA_FN_PARAMETER:
        {
            TypeName t = GetLastType(baseMem, def->type);
            if (IsUnsigned(t)) {
                mem[0].min.u = Mem<u64>(GetMinimumValueOfType(t).mem);
                mem[0].max.u = Mem<u64>(GetMaximumValueOfType(t).mem);
                mem[0].tag = RANGE_UNSIGNED;
            } else {
                mem[0].min.u = Mem<i64>(GetMinimumValueOfType(t).mem);
                mem[0].max.u = Mem<i64>(GetMaximumValueOfType(t).mem);
                mem[0].tag = RANGE_SIGNED;
            }
            return 1;
            break;
        }
    case SSA_UN_INIT:
        {
            TypeName t = GetLastType(baseMem, def->type);
            if (IsUnsigned(t)) {
                mem[0].min.u = Mem<u64>(GetMinimumValueOfType(t).mem);
                mem[0].max.u = Mem<u64>(GetMaximumValueOfType(t).mem);
                mem[0].tag = RANGE_UNSIGNED;
            } else {
                mem[0].min.u = Mem<i64>(GetMinimumValueOfType(t).mem);
                mem[0].max.u = Mem<i64>(GetMaximumValueOfType(t).mem);
                mem[0].tag = RANGE_SIGNED;
            }
            return 1;
            break;
        }
    case EXPRESSION_MEMORY_LOAD:
        {
            TypeName t = GetLastType(baseMem, def->type);
            if (IsUnsigned(t)) {
                mem[0].min.u = Mem<u64>(GetMinimumValueOfType(t).mem);
                mem[0].max.u = Mem<u64>(GetMaximumValueOfType(t).mem);
                mem[0].tag = RANGE_UNSIGNED;
            } else {
                mem[0].min.u = Mem<i64>(GetMinimumValueOfType(t).mem);
                mem[0].max.u = Mem<i64>(GetMaximumValueOfType(t).mem);
                mem[0].tag = RANGE_SIGNED;
            }
            return 1;
            break;
        }
    case EXPRESSION_MEMORY_STORE:
        {
            ASSERT(false);
            return 1;
        }
    case EXPRESSION_CALL:
        {
            TypeName t = GetLastType(baseMem, def->type);
            if (IsUnsigned(t)) {
                mem[0].min.u = Mem<u64>(GetMinimumValueOfType(t).mem);
                mem[0].max.u = Mem<u64>(GetMaximumValueOfType(t).mem);
                mem[0].tag = RANGE_UNSIGNED;
            } else {
                mem[0].min.u = Mem<i64>(GetMinimumValueOfType(t).mem);
                mem[0].max.u = Mem<i64>(GetMaximumValueOfType(t).mem);
                mem[0].tag = RANGE_SIGNED;
            }
            return 1;
            break;
        }
    case EXPRESSION_CONVERSION:
        {
            TypeName t = GetLastType(baseMem, def->type);
            u32 rangeCount = GetDefIntegerRanges(compiler, def->operand0, mem);
            if (t == TYPE_MODIFIER_POINTER || t == TYPE_MODIFIER_RESTRICTED_POINTER)
                return rangeCount;
            IntegerRange tmp[rangeCount];
            memcpy(tmp, mem, rangeCount * sizeof(IntegerRange));

            u32 alloc = 0;
            for (u32 i = 0; i < rangeCount; i++) {
                alloc += ConvertIntegerRange(tmp[i], t, mem + alloc);
            }
            return rangeCount;
            break;
        }
    case TOKEN_NEG:
        {
            u32 leftRangeCount = GetDefIntegerRanges(compiler, def->operand0, mem);
            for(u32 i = 0; i < leftRangeCount; i++) {
                if(mem[i].tag == RANGE_SIGNED) {
                    i64 max = 0 - mem[i].max.i;
                    i64 min = 0 - mem[i].min.i;
                    mem[i].min.i = Min(max, min);
                    mem[i].max.i = Max(max, min);
                }
                else if(mem[i].tag == RANGE_UNSIGNED) {
                    u64 max = 0 - mem[i].max.u;
                    u64 min = 0 - mem[i].min.u;
                    mem[i].min.u = Min(max, min);
                    mem[i].max.u = Max(max, min);
                }
            }
            break;
        }
    default:
        {
            u32 leftRangeCount = GetDefIntegerRanges(compiler, def->operand0, mem);
            u32 rightRangeCount = GetDefIntegerRanges(compiler, def->operand1, mem + leftRangeCount);

            for (u32 i = 0; i < leftRangeCount; i++) {
                for (u32 k = leftRangeCount; k < (leftRangeCount + rightRangeCount); k++) {

                    if (mem[k].tag == RANGE_ALL || mem[i].tag == RANGE_ALL) {
                        mem[i].tag = RANGE_ALL;
                    }

                    switch (def->opr) {
                    case TOKEN_PLUS:

                        switch (mem[i].tag) {
                        case RANGE_SIGNED:
                            mem[i].max.i = Clamp<i64>(mem[i].max.i + mem[k].max.i, I64_MAX, I64_MIN);
                            mem[i].min.i = Clamp<i64>(mem[i].min.i + mem[k].min.i, I64_MAX, I64_MIN);
                            break;
                        case RANGE_UNSIGNED:
                            mem[i].max.u = Clamp<u64>(mem[i].max.u + mem[k].max.u, U64_MAX, U64_MIN);
                            mem[i].min.u = Clamp<u64>(mem[i].min.u + mem[k].min.u, U64_MAX, U64_MIN);
                            break;
                        case RANGE_ALL:
                            break;
                        }
                        break;
                    case TOKEN_MINUS:
                        switch (mem[i].tag) {
                        case RANGE_SIGNED:
                            mem[i].max.i = Clamp<i64>(mem[i].max.i - mem[k].min.i, I64_MAX, I64_MIN);
                            mem[i].min.i = Clamp<i64>(mem[i].min.i - mem[k].max.i, I64_MAX, I64_MIN);
                            break;
                        case RANGE_UNSIGNED:
                            mem[i].max.u = Clamp<u64>(mem[i].max.u - mem[k].min.i, U64_MAX, U64_MIN);
                            mem[i].min.u = Clamp<u64>(mem[i].min.u - mem[k].max.i, U64_MAX, U64_MIN);
                            break;
                        case RANGE_ALL:
                            break;
                        }
                        break;
                    case TOKEN_ASTERISK:
                        switch (mem[i].tag) {
                        case RANGE_SIGNED:
                            {
                                i64 minMin = Clamp<i64>(mem[i].min.i * mem[k].min.i, I64_MAX, I64_MIN);
                                i64 maxMax = Clamp<i64>(mem[i].max.i * mem[k].max.i, I64_MAX, I64_MIN);
                                i64 minMax = Clamp<i64>(mem[i].min.i * mem[k].max.i, I64_MAX, I64_MIN);
                                i64 maxMin = Clamp<i64>(mem[i].max.i * mem[k].min.i, I64_MAX, I64_MIN);
                                mem[i].min.i = Min(Min(Min(minMin, minMax), minMax), maxMax);
                                mem[i].max.i = Max(Max(Max(minMin, minMax), minMax), maxMax);
                                break;
                            } 
                        case RANGE_UNSIGNED:
                            {
                                u64 minMin = Clamp<u64>(mem[i].min.u * mem[k].min.u, U64_MAX, U64_MIN);
                                u64 maxMax = Clamp<u64>(mem[i].max.u * mem[k].max.u, U64_MAX, U64_MIN);
                                u64 minMax = Clamp<u64>(mem[i].min.u * mem[k].max.u, U64_MAX, U64_MIN);
                                u64 maxMin = Clamp<u64>(mem[i].max.u * mem[k].min.u, U64_MAX, U64_MIN);
                                mem[i].min.u = Min(Min(Min(minMin, minMax), minMax), maxMax);
                                mem[i].max.u = Max(Max(Max(minMin, minMax), minMax), maxMax);
                                break;
                            } 
                        case RANGE_ALL:
                            break;
                        }
                        break;
                    case TOKEN_SLASH:
                        switch (mem[i].tag) {
                        case RANGE_SIGNED:
                            {
                                i64 minMin = Clamp<i64>(mem[i].min.i / mem[k].min.i, I64_MAX, I64_MIN);
                                i64 maxMax = Clamp<i64>(mem[i].max.i / mem[k].max.i, I64_MAX, I64_MIN);
                                i64 minMax = Clamp<i64>(mem[i].min.i / mem[k].max.i, I64_MAX, I64_MIN);
                                i64 maxMin = Clamp<i64>(mem[i].max.i / mem[k].min.i, I64_MAX, I64_MIN);
                                mem[i].min.i = Min(Min(Min(minMin, minMax), minMax), maxMax);
                                mem[i].max.i = Max(Max(Max(minMin, minMax), minMax), maxMax);
                                break;
                            } 
                        case RANGE_UNSIGNED: 
                            {
                                u64 minMin = Clamp<u64>(mem[i].min.u / mem[k].min.u, U64_MAX, U64_MIN);
                                u64 maxMax = Clamp<u64>(mem[i].max.u / mem[k].max.u, U64_MAX, U64_MIN);
                                u64 minMax = Clamp<u64>(mem[i].min.u / mem[k].max.u, U64_MAX, U64_MIN);
                                u64 maxMin = Clamp<u64>(mem[i].max.u / mem[k].min.u, U64_MAX, U64_MIN);
                                mem[i].min.u = Min(Min(Min(minMin, minMax), minMax), maxMax);
                                mem[i].max.u = Max(Max(Max(minMin, minMax), minMax), maxMax);
                                break;
                            }
                        case RANGE_ALL:
                            break;
                        }
                    }
                }
            }
            break;
        }
    }
}

bool IntRangeOverlap(IntegerRange r0, IntegerRange r1) {

    if (r0.tag == RANGE_ALL || r1.tag == RANGE_ALL)
        return true;

    if (r0.tag == RANGE_SIGNED) {
        if (r1.tag == RANGE_SIGNED) {
            return (r0.max.i <= r1.max.i && r0.max.i >= r1.min.i) || (r0.min.i <= r1.max.i && r0.min.i >= r1.min.i);
        } else if (r1.tag == RANGE_UNSIGNED) {
            return (r0.max.i <= r1.max.u && r0.max.i >= r1.min.u) || (r0.min.i <= r1.max.u && r0.min.i >= r1.min.u);
        }
    } else if (r0.tag == RANGE_UNSIGNED) {
        if (r1.tag == RANGE_SIGNED) {
            return (r0.max.u <= r1.max.i && r0.max.u >= r1.min.i) || (r0.min.u <= r1.max.i && r0.min.u >= r1.min.i);
        } else if (r1.tag == RANGE_UNSIGNED) {
            return (r0.max.u <= r1.max.u && r0.max.u >= r1.min.u) || (r0.min.u <= r1.max.u && r0.min.u >= r1.min.u);
        }
    }
}
bool MemoryClobbered(IntegerRange *memory, u32 memorySize, IntegerRange *clobbered, u32 clobberedSize) {

    for (u32 i = 0; i < memorySize; i++) {
        for (u32 k = 0; k < clobberedSize; k++) {
            if (IntRangeOverlap(memory[i], clobbered[k])) {
                return true;
            }
        }
    }
    return false;
}
void AddIntegerRanges(Compiler* compiler, IntegerRange* r0, IntegerRange* r1, u32 r0Count, u32 r1Count, TokenType opr) {
    
    for (u32 i = 0; i < r0Count; i++) {
        for (u32 k = 0; k < r1Count; k++) {

            if(r1[k].tag == RANGE_ALL || r0[i].tag == RANGE_ALL) {
                r0[i].tag = RANGE_ALL;
            }

            switch (opr) {
            case TOKEN_PLUS:
                switch (r0[i].tag) {
                case RANGE_SIGNED:
                    ASSERT(r1[k].tag == RANGE_SIGNED);
                    r0[i].max.i = Clamp<i64>(r0[i].max.i + r1[k].max.i, I64_MAX, I64_MIN);
                    r0[i].min.i = Clamp<i64>(r0[i].min.i + r1[k].min.i, I64_MAX, I64_MIN);
                    break;
                case RANGE_UNSIGNED:
                    ASSERT(r1[k].tag == RANGE_UNSIGNED);
                    r0[i].max.u = Clamp<u64>(r0[i].max.u + r1[k].max.u, U64_MAX, U64_MIN);
                    r0[i].min.u = Clamp<u64>(r0[i].min.u + r1[k].min.u, U64_MAX, U64_MIN);
                    break;
                case RANGE_ALL:
                    break;
                }break;
            case TOKEN_MINUS:
                switch (r0[i].tag) {
                case RANGE_SIGNED:
                    r0[i].max.i = Clamp<i64>(r0[i].max.i - r1[k].min.i, I64_MAX, I64_MIN);
                    r0[i].min.i = Clamp<i64>(r0[i].min.i - r1[k].max.i, I64_MAX, I64_MIN);
                    break;
                case RANGE_UNSIGNED:
                    r0[i].max.u = Clamp<u64>(r0[i].max.u - r1[k].min.i, U64_MAX, U64_MIN);
                    r0[i].min.u = Clamp<u64>(r0[i].min.u - r1[k].max.i, U64_MAX, U64_MIN);
                    break;
                case RANGE_ALL:
                    break;
                }
                break;
            }

        }
    }
}
u32 ConvertIntegerRange(IntegerRange range, TypeName t, IntegerRange *result) {

    i64 min = Mem<i64>(GetMinimumValueOfType(t).mem);
    u64 max = Mem<u64>(GetMaximumValueOfType(t).mem);

    u64 minAbs = Abs<i64>(min);
    u64 c = minAbs + max;

    if (range.tag == RANGE_SIGNED) {
        u32 c2 = range.max.i - range.min.i;
        if (c2 >= c) {
            result[0].min.i = min;
            result[0].max.u = max;
            result[0].tag = (min != 0) ? RANGE_SIGNED : RANGE_UNSIGNED;
            return 1;
        } else if (c2 == 0 || (range.max.i <= max && range.min.i >= min)) {
            result[0] = range;
            return 1;
        }

        Value v;

        if (IsUnsigned(t)) {

            Mem<i64>(v.mem) = range.min.i;
            v.type = TYPE_PRIMARY_INT64;
            v = ConvertValue(v, t);
            u64 wrap0 = Mem<i64>(v.mem);

            Mem<i64>(v.mem) = range.max.i;
            v.type = TYPE_PRIMARY_INT64;
            v = ConvertValue(v, t);
            u64 wrap1 = Mem<i64>(v.mem);

            u64 nMax = Max<u64>(wrap0, wrap1);
            u64 nMin = Min<u64>(wrap0, wrap1);

            result[0].min.u = min;
            result[0].max.u = nMin;

            result[1].min.u = nMax;
            result[1].max.u = max;

            result[0].tag = RANGE_UNSIGNED;
            result[1].tag = RANGE_UNSIGNED;
        } else {

            Mem<i64>(v.mem) = range.min.i;
            v.type = TYPE_PRIMARY_INT64;
            v = ConvertValue(v, t);
            i64 wrap0 = Mem<i64>(v.mem);

            Mem<i64>(v.mem) = range.max.i;
            v.type = TYPE_PRIMARY_INT64;
            v = ConvertValue(v, t);
            i64 wrap1 = Mem<i64>(v.mem);

            i64 nMax = Max<i64>(wrap0, wrap1);
            i64 nMin = Min<i64>(wrap0, wrap1);

            result[0].min.i = min;
            result[0].max.i = nMin;

            result[1].min.i = nMax;
            result[1].max.i = max;

            result[0].tag = RANGE_SIGNED;
            result[1].tag = RANGE_SIGNED;
        }

        return 2;
    } else {
        u32 c2 = range.max.u - range.min.u;
        if (c2 >= c) {
            result[0].min.i = min;
            result[0].max.u = max;
            result[0].tag = (min != 0) ? RANGE_SIGNED : RANGE_UNSIGNED;
            return 1;
        } else if (c2 == 0 || (range.max.i <= max && range.min.i >= min)) {
            result[0] = range;
            return 1;
        }

        Value v;
        if (IsUnsigned(t)) {

            Mem<i64>(v.mem) = range.min.u;
            v.type = TYPE_PRIMARY_INT64;
            v = ConvertValue(v, t);
            u64 wrap0 = Mem<i64>(v.mem);

            Mem<i64>(v.mem) = range.max.u;
            v.type = TYPE_PRIMARY_INT64;
            v = ConvertValue(v, t);
            u64 wrap1 = Mem<i64>(v.mem);

            u64 nMax = Max<u64>(wrap0, wrap1);
            u64 nMin = Min<u64>(wrap0, wrap1);

            result[0].min.u = min;
            result[0].max.u = nMin;

            result[1].min.u = nMax;
            result[1].max.u = max;

            result[0].tag = RANGE_UNSIGNED;
            result[1].tag = RANGE_UNSIGNED;
        } else {

            Mem<i64>(v.mem) = range.min.u;
            v.type = TYPE_PRIMARY_INT64;
            v = ConvertValue(v, t);
            i64 wrap0 = Mem<i64>(v.mem);

            Mem<i64>(v.mem) = range.max.u;
            v.type = TYPE_PRIMARY_INT64;
            v = ConvertValue(v, t);
            i64 wrap1 = Mem<i64>(v.mem);

            i64 nMax = Max<i64>(wrap0, wrap1);
            i64 nMin = Min<i64>(wrap0, wrap1);

            result[0].min.i = min;
            result[0].max.i = nMin;

            result[1].min.i = nMax;
            result[1].max.i = max;

            result[0].tag = RANGE_SIGNED;
            result[1].tag = RANGE_SIGNED;
        }
        return 2;
    }
}
void PrintIntRange(IntegerRange range) {
    switch (range.tag) {
    case RANGE_SIGNED:
        global_print("%c%i%s%i%s", '{', range.min.i,", ", range.max.i, "},");
        break;
    case RANGE_UNSIGNED:
        global_print("%c%i%s%i%s", '{', range.min.u,", ", range.max.u, "},");
        break;
    case RANGE_ALL:
        global_print("%s", "{-inf,+inf},");
        break;
    }
}
u32 GetDefValueRange(Compiler *compiler, u32 defPtr, ValueRange *mem) {

    byte *const baseMem = compiler->mem;
    SSADefinition *def = (SSADefinition *)(baseMem + defPtr);

    switch (def->opr) {
    case EXPRESSION_LITERAL: {
        LiteralExpr *litr = (LiteralExpr *)(baseMem + def->operand0);
        Value v = GetValueFromLiteral(litr->literal);
        mem[0].max = v;
        mem[0].min = v;
        mem[0].isMinInf = false;
        mem[0].isMaxInf = false;
        return 1;
    }
    case EXPRESSION_IMMEDIATE: {
        Value *v = (Value*)(baseMem + def->operand0);
        mem[0].max = *v;
        mem[0].min = *v;
        mem[0].isMinInf = false;
        mem[0].isMaxInf = false;
        return 1;
    }
    case SSA_PHI_NODE: {
        u32 *ptr = GetPhiOperandPtr(baseMem, def);
        u32 count = GetPhiOperandCount(def);
        u32 ret = 0;
        for (u32 i = 0; i < count; i++) {
            ret += GetDefValueRange(compiler, ptr[i], mem + ret);
        }
        return ret;
    }
    case SSA_FUNCTION: {

        Mem<u64>(mem[0].max.mem) = def->operand0;
        Mem<u64>(mem[0].min.mem) = def->operand0;
        mem[0].max.type = TYPE_PRIMARY_UINT64;
        mem[0].min.type = TYPE_PRIMARY_UINT64;
        mem[0].isMinInf = false;
        mem[0].isMaxInf = false;
        return 1;
    }
    case SSA_FN_PARAMETER: {
        TypeName t = GetLastType(baseMem, def->type);
        mem[0].max.type = t;
        mem[0].min.type = t;
        mem[0].isMinInf = true;
        mem[0].isMaxInf = true;
        return 1;
    }
    case SSA_UN_INIT: {
        TypeName t = GetLastType(baseMem, def->type);
        mem[0].max.type = t;
        mem[0].min.type = t;
        mem[0].isMinInf = true;
        mem[0].isMaxInf = true;
        return 1;
    }
    case EXPRESSION_MEMORY_LOAD: {
        TypeName t = GetLastType(baseMem, def->type);
        mem[0].max.type = t;
        mem[0].min.type = t;
        mem[0].isMinInf = true;
        mem[0].isMaxInf = true;
        return 1;
    }
    case EXPRESSION_MEMORY_STORE: {
        ASSERT(false);
        return 1;
    }
    case EXPRESSION_CALL: {
        TypeName t = GetLastType(baseMem, def->type);
        mem[0].max.type = t;
        mem[0].min.type = t;
        mem[0].isMinInf = true;
        mem[0].isMaxInf = true;
        return 1;
    }
    case EXPRESSION_CONVERSION: {
        u32 rangeCount = GetDefValueRange(compiler, def->operand0, mem);
        TypeName t = GetLastType(baseMem, def->type);
        for (u32 i = 0; i < rangeCount; i++) {
            if (!mem[i].isMaxInf) {
                mem[i].max = ConvertValue(mem[i].max, t);
            }
            if (!mem[i].isMinInf) {
                mem[i].min = ConvertValue(mem[i].min, t);
            }
        }
        return rangeCount;
    }
    default: {
        u32 leftRangeCount = GetDefValueRange(compiler, def->operand0, mem);
        u32 rightRangeCount = GetDefValueRange(compiler, def->operand1, mem + leftRangeCount);

        for (u32 i = 0; i < leftRangeCount; i++) {
            for (u32 k = leftRangeCount; k < leftRangeCount + rightRangeCount; k++) {
                switch (def->opr) {
                case TOKEN_PLUS:
                case TOKEN_MINUS:
                    mem[i].min = EvalOpr(mem[i].min, mem[k].min, (TokenType)def->opr, 3);
                    mem[i].max = EvalOpr(mem[i].max, mem[k].max, (TokenType)def->opr, 3);
                    mem[i].isMinInf |= mem[k].isMinInf;
                    mem[i].isMaxInf |= mem[k].isMaxInf;
                    break;
                case TOKEN_ASTERISK:
                case TOKEN_SLASH: {
                    Value minMin = EvalOpr(mem[i].min, mem[k].min, (TokenType)def->opr, 3);
                    Value maxMax = EvalOpr(mem[i].max, mem[k].max, (TokenType)def->opr, 3);
                    Value minMax = EvalOpr(mem[i].min, mem[k].max, (TokenType)def->opr, 3);
                    Value maxMin = EvalOpr(mem[i].max, mem[k].min, (TokenType)def->opr, 3);
                    mem[i].min = MinValue(MinValue(MinValue(minMin, minMax), minMax), maxMin);
                    mem[i].max = MaxValue(MaxValue(MaxValue(minMin, minMax), minMax), maxMin);
                    mem[i].isMinInf |= mem[k].isMinInf;
                    mem[i].isMaxInf |= mem[k].isMaxInf;
                    break;
                }
                }
            }
        }
        break;
    }
    }
}
void PrintRange(ValueRange r) {
    global_print("%c", '{');
    if (r.isMinInf)
        global_print("%s", "-inf");
    else
        PrintValue(r.min);

    global_print("%s", ", ");

    if (r.isMaxInf)
        global_print("%s", "+inf");
    else
        PrintValue(r.max);
    global_print("%s", "}, ");
}
bool RangeOverlap(ValueRange r0, ValueRange r1) {

    bool leftMaxBiggerOrEQRightMin;
    if (r0.isMaxInf || r1.isMinInf) {
        leftMaxBiggerOrEQRightMin = true;
    } else {
        leftMaxBiggerOrEQRightMin = (ValueBG(r1.min, r0.max) || ValuesEq(r1.min, r0.max));
    }

    bool leftMaxLessOrEQRightMax;
    if (r0.isMaxInf || r1.isMaxInf) {
        leftMaxLessOrEQRightMax = !(r0.isMaxInf && !r1.isMaxInf);
    } else {
        leftMaxLessOrEQRightMax = !ValueBG(r1.max, r0.max);
    }

    bool leftMinLessOrEQRightMax;
    if (r0.isMinInf || r1.isMaxInf) {
        leftMinLessOrEQRightMax = true;
    } else {
        leftMinLessOrEQRightMax = !ValueBG(r1.max, r0.min);
    }

    bool leftMinBiggerOrEQRightMin;
    if (r0.isMinInf || r1.isMinInf) {
        leftMinBiggerOrEQRightMin = r1.isMinInf || (r0.isMaxInf && r1.isMinInf);
    } else {
        leftMinBiggerOrEQRightMin = ValueBG(r1.min, r0.min) || ValuesEq(r1.min, r0.min);
    }

    return (leftMaxBiggerOrEQRightMin && leftMaxLessOrEQRightMax) ||
           (leftMinLessOrEQRightMax && leftMinBiggerOrEQRightMin);
}

bool RangesOverlap(ValueRange *r0, u32 r0Size, ValueRange *r1, u32 r1Size) {

    for (u32 i = 0; i < r0Size; i++) {
        for (u32 k = 0; k < r1Size; k++) {
            if (RangeOverlap(r0[i], r1[k]))
                return true;
        }
    }
    return false;
}
u32 GetClobberedMemoryRangesPhi(Compiler *compiler, u32 memPhi, IntegerRange *result) {

    if (memPhi == ~u32(0))
        return 0;
    byte *const baseMem = compiler->mem;

    u32 ret = 0;
    SSAMemoryDef *phi = (SSAMemoryDef *)(baseMem + memPhi);
    for (u32 i = 0; i < phi->predecessors.edgeCount; i++) {
        u32 predMemPtr = Mem<u32>(baseMem + phi->predecessors.edges + i * sizeof(u32));

        if(predMemPtr == ~u32(0)) {
            auto& r = result[ret++];
            r.tag = RANGE_ALL;
        }
        else {

            SSAMemoryDef *predMem = (SSAMemoryDef *)(baseMem + predMemPtr);
            switch (predMem->type) {
            case MEMORY_DEF:
                memcpy(result + ret, (IntegerRange *)(baseMem + predMem->addressRanges.edges), predMem->addressRanges.edgeCount * sizeof(IntegerRange));
                ret += predMem->addressRanges.edgeCount;
                break;
            case MEMORY_PHI:
                ret += GetClobberedMemoryRangesPhi(compiler, predMemPtr, result + ret);
                break;
            }
        }
    }
    return ret;
}
byte SSAMemoryEQOnRange(Compiler *compiler, IntegerRange* range, u32 rangeCount,u32 memoryPtr0, u32 memoryPtr1, u32 *visited) {

    if (memoryPtr0 == memoryPtr1) {
        return true;
    }
    byte* const baseMem = compiler->mem;

    visited[0] = 0;
    if (IsMemoryReachable(compiler, memoryPtr1, memoryPtr0, visited)) {

        u32 *result = visited;
        result[0] = 0;
        WalkMemory(compiler, memoryPtr0, memoryPtr1, result);
        u32 c = result[0];
        IntegerRange *loadRange = (IntegerRange *)(result + c + 1);

        bool clobbered = false;
        for (u32 i = 0; i < c; i++) {
            if(result[i] == ~u32(0)) continue;
            SSAMemoryDef *memory = (SSAMemoryDef *)(baseMem + result[i]);
            ASSERT(memory->type == MEMORY_DEF);
            clobbered |= MemoryClobbered(range, rangeCount, (IntegerRange *)(baseMem + memory->addressRanges.edges), memory->addressRanges.edgeCount);
        }
        return !clobbered;
    }

    visited[0] = 0;
    if (IsMemoryReachable(compiler, memoryPtr0, memoryPtr1, visited)) {

        u32 *result = visited;
        result[0] = 0;
        WalkMemory(compiler, memoryPtr1, memoryPtr0, result);
        u32 c = result[0];
        IntegerRange *loadRange = (IntegerRange *)(result + c + 1);

        bool clobbered = false;
        for (u32 i = 0; i < c; i++) {
            if(result[i] == ~u32(0)) continue;
            SSAMemoryDef *memory = (SSAMemoryDef *)(baseMem + result[i]);
            ASSERT(memory->type == MEMORY_DEF);
            clobbered |= MemoryClobbered(range, rangeCount, (IntegerRange *)(baseMem + memory->addressRanges.edges), memory->addressRanges.edgeCount);
        }
        return !clobbered;
    }
}

u32 GetCallsFromMemory(byte* baseMem, SSABasicBlock *block, SSADefinition *first, u32 memoryPtr, u32 *result, u32 *visited) {

    u32 ret = 0;
    SSADefinition *it = first;
    SSAMemoryDef* memory = (SSAMemoryDef*)(baseMem + memoryPtr);
    while (it != 0) {

        if (it->opr == EXPRESSION_CALL && it->extraPtr != ~u32(0)) {

            SSACallInfo* extra = (SSACallInfo*)(baseMem + it->extraPtr);
            u32 offset = 0;
            for(u32 i = 0; i < extra->memoryUseCount; i++) {
                auto observed = (RangeWithBase*)(extra->memoryUses + extra->memoryDefCount * sizeof(u32) + offset);
                if(observed->baseAddress == memory->basePtrDef) {
                    result[ret++] = (u64)it - (u64)baseMem;
                    break;
                }
                offset += sizeof(RangeWithBase) + observed->rangeCount * sizeof(IntegerRange);
            }
        }

        it = (it->nextDef == 0 ? nullptr : (SSADefinition *)(baseMem + it->nextDef));
    }

    for (u32 i = 0; i < block->successors.edgeCount; i++) {
        u32 succBlockPtr = Mem<u32>(baseMem + block->successors.edges + i * sizeof(u32));
        SSABasicBlock *succBlock = (SSABasicBlock *)(baseMem + succBlockPtr);

        bool blockVisited = false;
        u32 visitedCount = visited[0];
        for (u32 k = 1; k < visitedCount + 1; k++) {
            if (visited[k] == succBlockPtr) {
                blockVisited = true;
                break;
            }
        }
        if (blockVisited)
            continue;
        visited[(visited[0]++) + 1] = succBlockPtr;
        ret += GetCallsFromMemory(baseMem, succBlock, succBlock->firstDef, memoryPtr, result, visited);
    }
    return ret;
}
enum IntegerRangeEnum : byte {
    RANGE_NONE,
    
    RANGE_ALL,
    RANGE_SIGNED,
    RANGE_UNSIGNED,

    RANGE_COUNT,
};
struct IntegerRange {
    union {
        i64 i;
        u64 u;
    } max;
    union {
        i64 i;
        u64 u;
    } min;
    IntegerRangeEnum tag;
};
struct RangeWithBase {
    u32 baseAddress;
    u32 rangeCount;
    IntegerRange range[0];
};
struct ValueRange {
    Value min;
    Value max;
    bool isMinInf;
    bool isMaxInf;
};
*/