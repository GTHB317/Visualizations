#= The basic Circle structure, with fields 'p' for "predecessor" and 's' for "successor" to be used in the linked list structure for the 
boundary =#

mutable struct Circle
    name::String
    x::Float64
    y::Float64
    rad::Float64
    p::Union{Circle,Nothing}
    s::Union{Circle,Nothing}
end

#Functions for handling the linked list of circles that defines the boundary

function init_boundary(a::Array{Circle,1})
    for i in 1:length(a)-1
        a[i].s = a[i+1]
        a[i+1].p = a[i]
    end
    a[length(a)].s = a[1]
    a[1].p = a[length(a)]
end

function fwd_dist(c::Circle, d::Circle) #number of "s"'s required to move fwd from c to d
    count = 0
    circ = c
    while circ != d
        count += 1
        circ = circ.s
    end  
    return count
end

function insert_circle!(c::Circle,d::Circle,e::Circle)
    if (c.s != d)||(d.p != c)
        error("Two circles not adjacent.")
    else
        c.s = e
        e.p = c
        d.p = e
        e.s = d
    end
end

function fwd_remove!(c::Circle,d::Circle) #removes the segment between c,d, exclusive,  as one moves fwd
    if c == d
        error("Circles are the same.")
    elseif c.s == d
        error("Circles are consecutive.")
    else
        circ = c.s
        #removed = []
        while circ != d
            circ.p.s = circ.s
            circ.s.p = circ.p
            circ = circ.s            
        end        
    end        
end

#Functions related to the geometry of circles

function centre_dist(c::Circle) #distance from the centre of a circle to the origin
    return sqrt((c.x)^2 + (c.y)^2)
end

function fit_tang_circle!(C1::Circle,C2::Circle,C3::Circle) #calculates a centre for C3 s.t. it's tangent to C1, C2
    x1 = C1.x
    x2 = C2.x
    y1 = C1.y
    y2 = C2.y

    r1 = C1.rad
    r2 = C2.rad
    r = C3.rad
    
    dist = sqrt((x1 - x2)^2 + (y1 - y2)^2) #distance between the centers of C1 and C2
    
    if dist > r1 + r2 + 2*r
        error("Gap too large.")
    end
       
    #some trigonometry 
    cos_sig = (x2 - x1)/dist
    sin_sig = (y2 - y1)/dist
    cos_gam = (dist^2 + (r + r1)^2 - (r + r2)^2)/(2*dist*(r + r1))
    sin_gam = sqrt(1 - (cos_gam)^2)

    #centre coordinates of the tangent circle
    C3.x = x1 + (r + r1)*(cos_sig*cos_gam - sin_sig*sin_gam)
    C3.y = y1 + (r + r1)*(cos_sig*sin_gam + sin_sig*cos_gam)
    return C3
end

#Note: fit_tang_circle! fits C3 such that C1,C2,C3 are arranged counterclockwise

function place_starting_three(C1::Circle, C2::Circle, C3::Circle)
    #place centres of C1,C2 on x-axis, such that C1,C2 are tangent
    C1.x = -C1.rad
    C2.x = C2.rad

    #fit the third circle
    fit_tang_circle!(C2,C1,C3)  #it seems like it might be necessary to initialise with opposite orientation

    #calculate the centroid of their centers, and translate each circle by it
    (centroid_x,centroid_y) = (sum([C1.x, C2.x, C3.x])/3, sum([C1.y, C2.y, C3.y])/3)

    C1.x -= centroid_x
    C2.x -= centroid_x
    C3.x -= centroid_x

    C1.y -= centroid_y
    C2.y -= centroid_y
    C3.y -= centroid_y
end

#= Functions for the circle packing algorithm

    1)a) Locating the closest circle on the boundary to the origin, "Cm", and it's successor, "Cn". =#


function closest(c::Circle)  #finds the closest circle to the origin in the linked list containing c
    closest = c
    circ = c.s
    while circ != c
        if centre_dist(closest) > centre_dist(circ)
            closest = circ
        end
        circ = circ.s
    end
    return closest
end

#=    1)b) Locating the pair of succesive circles, c, c.s, with the following property: amongst all pairs of 
    succesive circles on the boundary, this pair minimises distance from the centre of d to the origin, when d is
    fitted tangent to this pair.  =#

function closest_place(c::Circle, d::Circle)
    closest = c
    circ = c.s
    while circ != c
        if centre_dist(fit_tang_circle!(closest, closest.s, d)) > centre_dist(fit_tang_circle!(circ, circ.s, d))
            closest = circ
        end
        circ = circ.s
    end
    return closest
end

#= 
    2) Checking for overlaps between a fitted circle and others on the boundary. =#

function do_intersect(c::Circle, d::Circle)     
    sqrt((c.x - d.x)^2 + (c.y - d.y)^2) < c.rad + d.rad  #could reformulate as discrepancy > threshold
end    

function geod_dist(Cm::Circle,Cn::Circle,C::Circle)   #convenience function to tidy up overlap_check
    return min(fwd_dist(Cn,C), fwd_dist(C,Cm))
end

function overlap_check(Cm::Circle,Cn::Circle,C::Circle)
    C_em = Cm
    C_en = Cn
    obstruct = []
    
    #collect circles that C intersects, if any
    circ = Cn.s
    while circ != Cm
        if do_intersect(circ,C)
            push!(obstruct,circ)
        end
        circ = circ.s
    end
    
    
    if length(obstruct) > 0                   #if there are any intersectiosn
        nearest = obstruct[1]                 #find the one closest to {Cm, Cn}, where distance is in number of steps
        for i in 1:length(obstruct)
            if geod_dist(Cm,Cn,obstruct[i]) < geod_dist(Cm,Cn,nearest)
                nearest = obstruct[i]
            end
        end
        if fwd_dist(Cn,nearest) <= fwd_dist(nearest,Cm)       #if the distance is realised fwd, change C_en
            C_en = nearest
            else                                              #if distance is realised bkwd and not fwd, change C_em
            C_em = nearest
        end
    end
    
    if (C_em,C_en) == (Cm,Cn)
        return "clear"
    else
        return C_em, C_en
    end
end

#= The circle layout function.

    It takes an input vector of radii, and returns a vector of centre coordinates of the corresponding circles in the layout. 
    Optional arguments are:
    "order": default = true
        if true it sorts the input vector in descending order before packing
    "try_place":  default is true
        if true the algorithm tries to place each new circle to be added to the packing as close to the origin as possilble,
        if false the algorithm tries to place each new circle to be added to the packing tangent to the closest circle on the boundary. 
=#

function circle_layout(input_rad_vec::Array{Float64,1}; order = true, try_place = true)
    if order
        input_rad_vec = reverse(sort(input_rad_vec))
    end
    
    # Initialise the circles with radii (not areas) as specified in input_rad_vec, and no boundary relations.
    circles = [Circle("Circle_$(i)", 0.0, 0.0, input_rad_vec[i],nothing,nothing) for i in 1:length(input_rad_vec)]
    
    #Taking care of "degenerate" cases when there are one or two circles
    
    if length(circles) == 1
        return [[c.x for c in circles],[c.y for c in circles],[c.rad for c in circles]]
    elseif length(circles) == 2
        circles[1].x = - circles[1].rad
        circles[2].x = circles[2].rad
        return [[c.x for c in circles],[c.y for c in circles],[c.rad for c in circles]]
    end

    # Place the first three circles to be mutually tangent, with centroid the origin.
    place_starting_three(circles[1], circles[2], circles[3])
    
    # Initialise the boundary    
    init_boundary(circles[1:3])
    
    #= for i in 1:length(circles)
        println(circles[i].name,":",circles[i].rad)
    end =#
    
    #Loop through the remaining circles, fitting them
    j = 4
    while j <= length(circles) 
        # Initial attempt to fit Circle_j
        if try_place
            cl = closest_place(circles[j-1], circles[j])
        else
            cl = closest(circles[j-1])
        end
        fit_tang_circle!(cl, cl.s, circles[j])
        
        # Check for overlaps and update, refit and recheck until "clear"
        check = overlap_check(cl, cl.s, circles[j]) 
        if check == "clear"
            insert_circle!(cl, cl.s, circles[j])
            j += 1
        else 
            while check != "clear" 
                Cm = check[1]
                Cn = check[2]
                fwd_remove!(Cm,Cn) 
                fit_tang_circle!(Cm,Cn,circles[j])
                check = overlap_check(Cm,Cn,circles[j])
                if check == "clear"
                    insert_circle!(Cm,Cn, circles[j])
                    j += 1
                end
            end
        end
    end
    return [[c.x for c in circles],[c.y for c in circles],[c.rad for c in circles] ]
end