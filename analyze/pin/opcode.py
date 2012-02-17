#!/usr/bin/env python3

import fileinput
from collections import OrderedDict


def group(name):
    lower = name.lower()
    upper = name.upper()
    if name in groups:
        return groups[name]
    elif lower in groups:
        return groups[lower]
    elif upper in groups:
        return name[upper]
    else:
        return 'Unknown'

ia='ARITH' #integer arithmetic
fa='ARITH' #floating point arithmetic
logic='LOGIC'
ctrl='CONTROL'
move='DATA'
other='OTHER'
convert='OTHER'

groups = OrderedDict(
    bt=logic,
    adc=ia,
    add=ia,
    addss=fa,
    AND=logic,
    andnpd=logic, #and not packed double
    andnps=logic, #and not packed single
    andpd=logic,
    andps=logic,
    addsd=fa,
    call_near=ctrl,
    cdqe=convert, #convert doubleword to quadword
    cld=other,  #clear direction flag
    cmovnle=move,
    cmovele=move,
    cmovenbe=move,
    cmovenp=move,
    cmovns=move,
    cmovb=move,
    cmovbe=move,
    cmovl=move,
    cmovnb=move,
    cmovnl=move,
    cmovs=move,
    cmovnz=move,
    cmovz=move,
    cmovle=move,
    cmovnbe=move,
    cmovnp=move,
    cvtsi2sd=convert, #convert signed integer to scalar double
    cvtsi2ss=convert,
    cvttsd2si=convert, #convert with truncation scalar double to signed integer
    cvttss2si=convert,
    cvtpd2ps=convert,
    cvtps2pd=convert,
    cvtsd2ss=convert,
    cvtss2sd=convert,
    cwde=convert,
    cmp=logic,
    cmpsb=logic, #compare string operands
    cmpsd_xmm=logic, #compare scalar double
    cmpss=logic,
    cqo=move,
    dec=ia,
    div=ia,
    divsd=fa,
    divss=fa,
    fld=move, #floating point load
    fld1=move,
    fnstsw=move, #store x87 fpu environment
    fprem=fa,    #floating point partial remainder
    fstp=move,   #floating point store
    idiv=ia,
    imul=ia,
    inc=ia,
    jb=ctrl,
    jbe=ctrl,
    jl=ctrl,
    jle=ctrl,
    jnb=ctrl,
    jnbe=ctrl,
    jnle=ctrl,
    jns=ctrl,
    jnz=ctrl,
    jnl=ctrl,
    jz=ctrl,
    jmp=ctrl,
    jnp=ctrl,
    jp=ctrl,
    js=ctrl,
    lea=ia,
    leave=ctrl,
    maxsd=logic, #max
    maxss=logic,
    minsd=logic,
    minss=logic,
    movapd=move,
    movaps=move,
    movq=move,
    movsb=move,
    movsd=move,
    movsq=move,
    mov_mr=move,
    mov_rm=move,
    mov_rr=move,
    movsd_xmm=move,
    movss=move,
    movsx=move,
    movsxd=move,
    movzx=move,
    mulsd=fa,
    mul=ia, #unsigned multiply
    mulss=fa,
    neg=ia,
    NOT=logic,
    nop=other,
    OR=logic,
    orpd=logic,
    orps=logic,
    pop=move,
    push=move,
    rol=logic, #rotate
    ror=logic,
    ret_near=ctrl,
    sar=ia,
    scasb=logic, #scan string
    sbb=ia,    #integer subtraction with borrow
    setb=move,
    setbe=move,
    setl=move,
    setle=move,
    setnb=move,
    setnbe=move,
    setnl=move,
    setnp=move,
    setnle=move,
    setnz=move,
    setp=move,
    sets=move,
    setz=ctrl,
    shl=ia,
    shr=ia,
    sqrtsd=fa,
    sqrtss=fa,
    stosq=move,
    stosb=move, #store string
    stosd=move,
    stosw=move,
    sub=ia,
    subsd=fa,
    subss=fa,
    test=logic,
    ucomisd=logic, #unordered compare scalardouble
    ucomiss=logic,
    unpcklpd=move, #unpack and interleave
    unpcklps=move,
    xor=logic,
    xorpd=logic,    #xor packed double precision
    xorps=logic
    )


if __name__ == "__main__":
    for line in fileinput.input():
        if line == '\n' or line.startswith('#'):
            continue
        try:
            op = line.split()[1]
            if op.startswith('*'):
                continue
            print("{:15} {:>10}".format(op, group(op)))
        except IndexError:
            print('bad line: '+line)
            raise
    
