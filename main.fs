module Assignment

type AMPM = AM | PM

// This function checks if an hour value `h` is not in [1,12] range
let areHoursInvalid h =
    if h < 1 || h > 12

// This function checks if a minute value `m` is not in [0,59] range
let areMinutesInvalid m =
    if m < 0 || m > 59 then
        true
    else
        false

// This function creates a valid time tuple
//      use above functions: areHoursInvalid & areMinutesInvalid
let time h m ampm :(int * int * AMPM) =
    let hour = 
        if areHoursInvalid h then
            12
        else
            h
  let minute = if areMinutesInvalid m then 0 else m
    (hour, minute, ampm)

// This function compares two times in tuple format
let lessThan (time1: int * int * AMPM) (time2: int * int * AMPM) :bool =
    let h1, m1, p1 = time1
    let h2, m2, p2 = time2
    //if p1 < p2 then
    if p1 = AM && p2 = PM then
        true
    else
        if p1 = p2 && h1 = h2 && m1 < m2 then
            true
        else if m1 < m2 then
            true
        else
