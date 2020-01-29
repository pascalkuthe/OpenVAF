/// An Ast representing a parser Verilog-AMS project (root file);
/// It provides stable indicies for every Node because the entire is immutable once created;
/// It uses preallocated constant size arrays for performance so you should box this as this is a lot of data to put on the stack

//TODO make this into a general proc macro with lifetimes like compact arena
pub struct Hir<'tag> {
    //TODO unsized
    //TODO configure to use different arena sizes
    //Declarations
    //    parameters: NanoArena<'tag,Parameter>,
    //    nature: NanoArena<'tag,Nature>
    branches: NanoArena<'tag, AttributeNode<'tag, BranchDeclaration>>,
    nets: TinyArena<'tag, AttributeNode<'tag, Net>>,
    ports: NanoArena<'tag, AttributeNode<'tag, Port>>,
    variables: TinyArena<'tag, AttributeNode<'tag, Variable<'tag>>>,
    modules: NanoArena<'tag, AttributeNode<'tag, Module<'tag>>>,
    functions: NanoArena<'tag, AttributeNode<'tag, Function<'tag>>>,
    disciplines: NanoArena<'tag, AttributeNode<'tag, Discipline>>,
    //Ast Items
    expressions: TinyArena<'tag, Node<Expression<'tag>>>,
    blocks: NanoArena<'tag, AttributeNode<'tag, SeqBlock<'tag>>>,
    attributes: TinyArena<'tag, Attribute>,
    statements: TinyArena<'tag, Statement<'tag>>,
    pub top_nodes: Vec<TopNode<'tag>>, //would prefer this to be stored here instead of somewhere else on the heap but its probably fine for now
    pub top_symbols: SymbolTable<'tag>,
}
///this module contains copys of the dfinitions of tiny/small arena so we are able to acess internal fields for initialisation on the heap using pointers

impl<'tag> Ast<'tag> {
    /// # Safety
    /// You should never call this yourself use mk_ast! instead!
    /// The tag might not be unique to this arena otherwise which would allow using ids from a different arena which is undfined behavior;
    /// Apart from that this function should be safe all internal unsafe functions calls are there to allow
    pub unsafe fn new(_tag: InvariantLifetime<'tag>) -> Box<Self> {
        let layout = std::alloc::Layout::new::<Self>();
        #[allow(clippy::cast_ptr_alignment)]
        //the ptr cast below has the right alignment since we are allocation using the right layout
        let mut res: NonNull<Ast<'tag>> = NonNull::new(std::alloc::alloc(layout) as *mut Self)
            .unwrap_or_else(|| std::alloc::handle_alloc_error(layout));
        NanoArena::init(&mut res.as_mut().branches);
        TinyArena::init(&mut res.as_mut().nets);
        NanoArena::init(&mut res.as_mut().ports);
        TinyArena::init(&mut res.as_mut().variables);
        NanoArena::init(&mut res.as_mut().modules);
        NanoArena::init(&mut res.as_mut().functions);
        NanoArena::init(&mut res.as_mut().disciplines);
        TinyArena::init(&mut res.as_mut().expressions);
        NanoArena::init(&mut res.as_mut().blocks);
        TinyArena::init(&mut res.as_mut().attributes);
        TinyArena::init(&mut res.as_mut().statements);
        std::ptr::write(&mut res.as_mut().top_nodes, Vec::with_capacity(64));
        std::ptr::write(
            &mut res.as_mut().top_symbols,
            SymbolTable::with_capacity(64),
        );

        Box::from_raw(res.as_ptr())
    }
}
