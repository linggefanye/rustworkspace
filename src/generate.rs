use rand::{
    Rng,
    rngs::StdRng,
    SeedableRng,
};

#[derive(Debug, Clone)]
pub struct Target {
    syscalls: Vec<Syscall>,
    resources: Vec<Resource>,

    //...
}

#[derive(Debug, Clone)]
pub struct Resource {
    name: String,
    base_type: BaseType,
    consts: Vec<u64>,
    type_options: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
enum BaseType {
    Int8,
    Int16,
    Int32,
    Int64,
    IntPtr,
    Custom(String),
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
    //...
}

#[derive(Debug, Clone)]
pub struct Syscall {
    pub name: String,
    pub args: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Field {
    name: String,
    typename: String,
    type_options: Option<Vec<String>>,
}

#[derive(Debug, Clone)]
pub struct Argument {
    typename: String,
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
    //let s = State::new(target, ct, None);

    while p.calls.len() < ncalls {
        let calls = generate_call(&mut rng, /* &s, */ p.calls.len(), &mut p);
        for c in calls {
            /* s.analyze(c); */
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
    /* s: &State, */
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
    generate_particular_call(rng,/*  s, */ meta)
}

fn generate_particular_call<R: Rng>(
    rng: &mut R,
   /*  s: &State, */
    meta: &Syscall,
) -> Vec<Call> {
    /* if meta.attrs.disabled {
        panic!("generating disabled call {}", meta.name);
    }
    if meta.attrs.no_generate {
        panic!("generating no_generate call: {}", meta.name);
    } */
    
    let mut c = make_call(meta, Vec::new());
    let (args, mut calls) = generate_args(rng,/*  s,  */&meta.args, Dir::DirIn);
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
    /* s: &State, */
    fields: &Vec<Field>,
    dir: Dir,
) -> (Vec<Argument>, Vec<Call>) {
    let mut calls = Vec::new();
    let mut args = Vec::with_capacity(fields.len());

    // Generate all args. Size args have the default value 0 for now.
    for field in fields {
        let (arg, mut calls1) = generate_arg(rng, &field.typename, dir.clone());
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
fn generate_arg<R: Rng>(
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



fn main() {
    let syscall = Syscall {
        name: "syz_test_write".to_string(),
        args: vec![
            Field {
                name: "count".to_string(),
                typename: "Int8".to_string(),
                type_options: Some(vec![]),
            },
        ],
    };

    let target = Target {
        syscalls: vec![syscall.clone()],
        resources: vec![],
        // ...
    };

    let prog = generate(&target, 12345, 1);

    println!("Generated Prog: {:#?}", prog);

    // Test if the generated program has the expected syscall
    for call in &prog.calls {
        if call.meta.name == syscall.name {
            println!("Found expected syscall: {}", syscall.name);
        }
    }
}