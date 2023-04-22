use rand::{
    Rng;|
    rngs::StdRng;
    SeedableRng;
}

#[derive(Debug)]
struct Target {
    syscalls: Vec<Syscall>,
    resources: Vec<Resource>,

    //...
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

#[derive(Debug)]
pub struct SysCall {
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
    size: u64,
}

#[derive(Debug)]
enum Dir {
    DirIn,
    DirOut,
    DirInOut,
}

pub fn generate(target: &Target, rs: u64, ncalls: usize/* , ct: Option<&ChoiceTable> */) -> Prog {
    let mut p = Prog {
        target: target,
    };
    let mut rng = StdRng::seed_from_u64(rs);
    //let s = State::new(target, ct, None);

    while p.calls.len() < ncalls {
        let calls = generate_call(&mut rng, /* &s, */ &mut p, p.calls.len());
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
    p: &mut Prog,
    insertion_point: usize,
) -> Vec<Call> {
    // Implementation of generate_call goes here.
    // ...
}
