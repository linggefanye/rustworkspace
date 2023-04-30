use rand::{
    Rng,
    rngs::StdRng,
    SeedableRng,
};

use std::collections::HashMap;
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub struct Target {
    syscalls: Vec<Syscall>,
    resources: Vec<Resource>,

    page_size: u64,
	num_pages: u64,

    resource_map: HashMap<String, Resource>,
    resource_ctors: HashMap<String, Vec<Syscall>>, 

    //...
}

impl Target{
    pub fn is_compatible_resource(&self, dst:String, src: String) -> bool {
        
        let dst_res = self.resource_map.get(&dst).unwrap();
        let src_res = self.resource_map.get(&src).unwrap();

        is_compatible_resource_impl(&dst_res.kind, &src_res.kind)
    }

    fn is_compatible_resource_impl(dst_kind: &Vec<String>, src_kind: &Vec<String>/* , precise: bool */) -> bool {
        if dst_kind.len() > src_kind.len() {
            // Destination resource is more specialized, e.g dst=socket, src=fd.
            /* if precise {
                return false;
            } */
        } else if src_kind.len() > dst_kind.len() {
            // Source resource is more specialized, e.g dst=fd, src=socket.
            let src_kind = &src_kind[..dst_kind.len()];
        }
    
        for (k_dst, k_src) in dst_kind.iter().zip(src_kind) {
            if k_dst != k_src {
                return false;
            }
        }
    
        true
    }

    /* pub fn assign_sizes_call(c: Call) */
}

#[derive(Debug)]
struct Resource {
    name: String,
    kind: Vec<String>,
    values: Vec<u64>,
    ctors: Vec<i8>,
}

#[derive(Debug, Clone)]
enum BaseType {
    Int8,
    Int16,
    Int32,
    Int64,
    IntPtr,
    ResourceType(Resource),
    PointerType(Argument),
    Custom(String),
}

impl BaseType {
    pub fn generate_arg<R: Rng>(&self, rng: &mut R, s:&State, dir:Dir, to: &Option<Vec<String>>)-> (Argument, Vec<Call>){
        match self {
            BaseType::Int8 =>{
            	let to1 = to.as_ref();
                let tovec = to1.unwrap();
                let mut constval = 0;
                if tovec.is_empty() {
                    constval = rng.gen::<u8>() as u64;
                }
                else if tovec.len() == 1 {
                    let (start, max) = parse_to(tovec.get(0).unwrap());
                    constval = rng.gen_range(start..max) as u64;
                }else if tovec.len() == 2{
                    let (start, max) = parse_to(tovec.get(0).unwrap());
                    let align = tovec.get(1).unwrap().parse::<u64>().unwrap();
                    constval = (rng.gen_range(0..(max - start)/align) * align + start) as u64;
                }
                else{
                    panic!("Unexpected type options");
                };
                (       
                    Argument{
                        typename: BaseType::Int8,
                        dir: dir,
                        bitsize: 8,
                        val: constval,
                    },
                    Vec::new(),
                )
            }
            BaseType::Int16 =>(            
                Argument{
                    typename: BaseType::Int16,
                    dir: dir,
                    bitsize: 16,
                    val: rng.gen::<u16>() as u64,
                },
                Vec::new(),
            ),
            BaseType::Int32 =>(            
                Argument{
                    typename: BaseType::Int32,
                    dir: dir,
                    bitsize: 32,
                    val: rng.gen::<u32>() as u64,
                },
                Vec::new(),
            ),
            BaseType::Int64 =>(            
                Argument{
                    typename: BaseType::Int64,
                    dir: dir,
                    bitsize: 64,
                    val: rng.gen(),
                },
                Vec::new(),
            ),
            BaseType::ResourceType =>{
                let (arg, calls) = create_resource(rng, s, self, dir);
                (
                    arg,
                    calls,
                )
            }
            _ => panic!("Unknown BaseType"),
        }

    }

}

fn create_resource<R: Rng>(
    rng: &mut R,
    s: &State,
    res: &BaseType::ResourceType,
    dir: Dir,
) -> (ResultArg, Vec<Call>) {
    let kind = res.desc.name.clone();
    let metas = enabled_ctors(s, &kind);

    /* if metas.is_empty() {
        return (ResultArg::make_result_arg(None, 0), vec![]);
    } */

    let meta = &metas[rng.gen_range(0..metas.len())];
    let calls = generate_particular_call(rng, s, meta);
    let mut s1 = State::new(s.target.clone());
    s1.analyze(&calls[calls.len() - 1]);

    let mut all_res: Vec<RefCell<ResultArg>> = Vec::new();
    for (kind1, res1) in s1.resources.iter() {
        if s.target.is_compatible_resource(&kind, kind1) {
            all_res.extend(res1.iter().cloned());
        }
    }

    /* all_res.sort_by(|a, b| a.borrow().type_().name().cmp(&b.borrow().type_().name())); */

    /* if all_res.is_empty() {
        panic!(
            "failed to create a resource {:?} ({:?}) with {:?}",
            res.Resource.kind[0], kind, meta.name
        );
    } */

    let arg = ResultArg::make_result_arg(
        Some(all_res[rng.gen_range(0..all_res.len())].clone()),
        0,
    );
    (arg, calls)
}


fn parse_to(input: &str) -> (u64, u64) {
    let parts: Vec<&str> = input.trim().split(':').collect();
    let start = parts[0].parse::<u64>().unwrap();
    let max = parts[1].parse::<u64>().unwrap();
    (start, max)
}


fn enabled_ctors(s: &State, kind: String) -> Vec<Syscall>{
    let mut metas: Vec<Syscall> = Vec::new();

    if let Some(ctors) = s.resource_ctors.get(kind) {
        for meta in ctors {
            metas.push(meta.clone());
        }
    }
    metas

}

#[derive(Debug, Clone)]
pub struct Prog {
    pub target: Target,
    pub calls: Vec<Call>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub meta: Syscall,
    pub args: Vec<Argument>,
    pub ret: ResultArg,
    //...
}

#[derive(Debug, Clone)]
pub struct Syscall {
    pub name: String,
    pub args: Vec<Field>,
    pub ret: Option<BaseType>,
}

#[derive(Debug, Clone)]
pub struct Field {
    name: String,
    typename: BaseType,
    type_options: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
pub struct ResultArg {
    // ... 其他成员变量

    res: Option<RefCell<ResultArg>>,
    /* oppdiv: u64,
    oppadd: u64, */
    val: u64,
    uses: HashMap<RefCell<ResultArg>, bool>,
}

impl ResultArg {
    // 创建一个新的 ResultArg
    fn make_result_arg(/* t: BaseType, dir: Dir, */ r: Option<RefCell<ResultArg>>, v: u64) -> Self {
        let mut result_arg = ResultArg {
            res: None,
            val: v,
            uses: HashMap::new(),
        };
        if let Some(rc_res) = r {
            result_arg.res = Some(rc_res);
            if rc_res.borrow().uses.is_empty() {
                rc_res.borrow_mut().uses = HashMap::new();
            }
            rc_res.borrow_mut().uses.insert(RefCell::new(result_arg.clone()), true);
        }
        result_arg
    }

    // 添加使用这个 ResultArg 的其他 ResultArg
    fn add_use(&mut self, other: &RefCell<ResultArg>) {
        self.uses.insert(other.clone(), true);
    }

    // 移除使用这个 ResultArg 的其他 ResultArg
    fn remove_use(&mut self, other: &RefCell<ResultArg>) {
        self.uses.remove(other);
    }
}

#[derive(Debug, Clone)]
pub struct State{
    pub target: Target,
    pub corpus: Vec<Prog>,
    pub resources: HashMap<String, Vec<RefCell<ResultArg>>>,
    pub ma: MemAlloc,
}

impl State {
    pub fn new(target: Target/* , corpus: Vec<prog> */) -> self{
        State{
            target: target,
            corpus: vec![],
            resource: HashMap::new(),
            ma: MemAlloc::new(target.page_size*target.num_pages).expect("Failed to create MemAlloc instance"),
        }
    }

    pub fn analyze(&mut self, c: Call){

    }
}

pub struct MemAlloc {
    size: u64,
    mem: [[u64; MEM_ALLOC_L0_SIZE]; MEM_ALLOC_L1_SIZE],
    buf: [u64; MEM_ALLOC_L0_SIZE],
}

impl MemAlloc {
    pub fn new(total_mem_size: u64) -> Result<Self, String> {
        if total_mem_size > MEM_ALLOC_MAX_MEM {
            return Err(format!(
                "new_mem_alloc: too much mem {} (max: {})",
                total_mem_size, MEM_ALLOC_MAX_MEM
            ));
        }
        if total_mem_size % MEM_ALLOC_L0_MEM != 0 {
            return Err(format!(
                "new_mem_alloc: unaligned size {} (align: {})",
                total_mem_size, MEM_ALLOC_L0_MEM
            ));
        }
        let size = total_mem_size / MEM_ALLOC_GRANULE;
        let mut mem = vec![None; MEM_ALLOC_L1_SIZE];
        let buf = vec![0u64; MEM_ALLOC_L0_SIZE];
        mem[0] = Some(buf.clone());
        Ok(Self { size, mem, buf })
    }
}


const MEM_ALLOC_GRANULE: u64 = 64;
const MEM_ALLOC_MAX_MEM: u64 = 16 << 20;
const MEM_ALLOC_L0_SIZE: u64 = 64;
const BITS_PER_U64: u64 = 8 * 8;
const MEM_ALLOC_L0_MEM: u64 = MEM_ALLOC_L0_SIZE * MEM_ALLOC_GRANULE * BITS_PER_U64;
const MEM_ALLOC_L1_SIZE: u64 = MEM_ALLOC_MAX_MEM / MEM_ALLOC_L0_MEM;

#[derive(Debug, Clone)]
pub struct Argument {
    typename: BaseType,
    dir: Dir,
    bitsize: u64,
    val: u64,
}

#[derive(Debug, Clone)]
enum Dir {
    DirIn,
    DirOut,
    DirInOut,
}

pub fn generate(target: &Target, rs: u64, ncalls: usize/* , ct: Option<&ChoiceTable> */) -> Prog {
    let mut p = Prog {
        target: target.clone(),
        calls: Vec::new(),
    };
    let mut rng = StdRng::seed_from_u64(rs);
    let s = State::new(target);

    while p.calls.len() < ncalls {
        let calls = generate_call(&mut rng, &s, p.calls.len(), &mut p);
        for c in calls {
            s.analyze(c);
            p.calls.push(c);
        }
    }

    /* while p.calls.len() > ncalls {
        p.remove_call(ncalls - 1);
    } */

    /* p.sanitize_fix();
    p.debug_validate(); */
    p
}

fn generate_call<R: Rng>(
    rng: &mut R,
    s: &State, 
    insertion_point: usize,
    p: &mut Prog,
) -> Vec<Call> {
    /* let mut bias_call = -1;
    if insertion_point > 0 {
        // Choosing the base call is based on the insertion point of the new calls sequence.
        let insertion_call = &p.calls[rng.gen_range(0..insertion_point)].meta;
        if !insertion_call.attrs.no_generate {
            // We must be careful not to bias towards a non-generatable call.
            bias_call = insertion_call.id;
        }
    } */
    let syscall_count = p.target.syscalls.len(); // Replace with the appropriate method to get the number of syscalls
    let idx = rng.gen_range(0..syscall_count);
    let meta = &p.target.syscalls[idx];
    generate_particular_call(rng, s, meta)
}

fn generate_particular_call<R: Rng>(
    rng: &mut R,
    s: &State,
    meta: &Syscall,
) -> Vec<Call> {
    /* if meta.attrs.disabled {
        panic!("generating disabled call {}", meta.name);
    }
    if meta.attrs.no_generate {
        panic!("generating no_generate call: {}", meta.name);
    } */
    
    let mut c = make_call(meta, Vec::new());
    let (args, mut calls) = generate_args(rng, s, &meta.args, Dir::DirIn);
    c.args = args;
    /* s.target.assign_sizes_call(&mut c); */
    calls.push(c);

    calls
}

fn make_call(meta: &Syscall, args: Vec<Argument>) -> Call {
    Call {
        meta: meta.clone(),
        args: args.clone(),
    }
}


fn generate_args<R: Rng>(
    rng: &mut R,
    s: &State,
    fields: &Vec<Field>,
    dir: Dir,
) -> (Vec<Argument>, Vec<Call>) {
    let mut calls = Vec::new();
    let mut args = Vec::with_capacity(fields.len());

    // Generate all args. Size args have the default value 0 for now.
    for field in fields {
        let (arg, mut calls1) = field.typename.generate_arg(rng, s, dir.clone(), &field.type_options);
        /* if arg.is_none() {
            panic!(
                "generated arg is nil for field '{}', fields: {:?}",
                field.typename, fields
            );
        } */
        args.push(arg);
        calls.append(&mut calls1);
    }

    (args, calls)
}

// Stub implementation of generate_arg function
/* fn generate_arg<R: Rng>(
    rng: &mut R,
    typename: &String,
    dir: Dir,
) -> (Argument, Vec<Call>) {
    // ...
    match typename.as_str(){
        "Int8" => (
            Argument{
                typename: "Int8".to_string(),
                dir: dir,
                bitsize: 8,
                val: rng.gen::<u8>() as u64,
            },
            Vec::new()
        ),
        _ => panic!(),
    }
}
 */


fn main() {
    let syscall = Syscall {
        name: "syz_test_write".to_string(),
        args: vec![
            Field {
                name: "count".to_string(),
                typename: BaseType::Int8,
                type_options: Some(vec!["0:20".to_string(),"2".to_string()]),
            },
        ],
    };

    let target = Target {
        syscalls: vec![syscall.clone()],
        resources: vec![],
        // ...
    };

    let prog = generate(&target, rand::thread_rng().gen(), 1);

    println!("Generated Prog: {:#?}", prog);

    // Test if the generated program has the expected syscall
    for call in &prog.calls {
        if call.meta.name == syscall.name {
            println!("Found expected syscall: {}", syscall.name);
        }
    }
}