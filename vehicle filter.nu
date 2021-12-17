let vehicle = (dataframe open Motor_Vehicle_Collisions_-_Crashes.csv)

filterVehicle ($vehicle) "1"
filterVehicle ($vehicle) "2"
filterVehicle ($vehicle) "3"
filterVehicle ($vehicle) "4"
filterVehicle ($vehicle) "5"


# Documentation for filterVehicle
def filterVehicle [df_origin, name] {

    let $df = ($df_origin | dataframe column $"VEHICLE TYPE CODE ($name)")


    # Write some code here
    let truck = ($df | dataframe contains "(?i)(18 wh)|(18 wheeler)|(26ft)|(35 ft)|(4 axe)|(armor)|(armored truck)|(back)|(backh)|(beverage truck)|(boom)|(box t)|(boxtr)|(bucke)|(bulk agriculture)|(bulld)|(cargo)|(catapillar)|(cemen)|(cm)|(comix)|(con ed tru)|(concrete mixer)|(cat)|(const)|(cont)|(crane)|(delivery t)|(dhl t)|(dilevery t)|(drill rig)|(dp)|(dump)|(e one)|(escavator)|(excav)|(farm vehicle)|(freightlin)|(^fr)|(freig)|(frht)|(frieg)|(front-load)|(front)|(fuel)|(garba)|(grain)|(heavy)|(hino)|(hopper)|(kenwo)|(large com veh(6 or more tires))|(liebh)|(mac t)|(mack)|(work truck)|(well driller)|(uhual)|(uhal)|(u-hau)|(u_hal)|(u haul tru)|(u hau)|(trk)|(tram)|(tract)|(trac.)|(trac)|(tr)|(toter)|(tow)|(tl)|(tk)|(tir)|(tank)|(stake)|(snow)|(skid loade)|(shove)|(sgws)|(semi)|(sanit)|(road sweep)|(rescu)|(quality tr)|(power shov)|(parce)|(nyc sanita)|(nt tr)|(moving tru)")

    let amb = ($df | dataframe contains "(?i)(ABULA)|(^amb$)|(ambu)|(amulance)|(E.M.S)|(EMERGENCY)|(EMRGN)|(EMS)|(fdny)|(GEN  AMBUL)|(G AMB)|(white ambu)|(NYS AMBULA)|(NYS A)|(nyc EMS)|(Fd fi)|(fd tr)|(FD tr)|(FD TR)|(fire)|(FIRER)|(firet)|(FIRTRUCK)|(KME/F)|(ladde)|(LADDER)|(nyfd)|(NYC FD)|(NYC F)")

    let bike = ($df | dataframe contains "(?i)(2 WHE)|(Bicyc)|(BICYCLE)|(Bike)|(BK)|(E BIK)|(E bike)|(E REVEL SC)|(e sco)|(E SCO)|(E scooter)|(e-bik)|(E-Bik)|(E-bike)|(E-Bike)|(E-BIKE)|(E-MOT)|(E-Sco)|(E-scooter\t)|(E-Scooter)|(E-UNICYCLE)|(ebike)|(Ebike)|(EBIKE)|(ELECTRIC B)|(ELEC. UNIC)|(kick scoot)|(segwa)|(scooter)|(scooter GA)|(scoot)|(scoo)|(PUSH SCOOT)|(pallet)")

    let motor = ($df | dataframe contains "(?i)(50cc Scoot)|(atv p (heavy))|(gas mo ped)|(GAS POWERE)|(gas scoote)|(Gas scoote)|(Yamaha)|(SPARK150 S)|(servi)|(Pavin)|(motorscooter)|(motorscoot)|(motorcycle)|(motorbike)|(motor scoo)|(motor)|(moped)|(mopd)|(mopad)|(MO PE)|(Mo pa)|(Minicycle)|(Minibike)")

    let commercial = ($df | dataframe contains "(?i)(^co$)|(^com$)|(^com v$)|(^comer$)|(^comm$)|(^commercial$)|(^commerical$)|(^fleet$)|(^commm$)|(^commu$)|(^cab$)|(^cabin$)|(^deiv$)|(^del$)|(^delivery$)|(delv)|(^fed e$)|(^fedex$)|(^fedx$)|(^grumm)|(^mail$)|(^mail truck$)|(^taxi$)|(^usps2$)|(^usps/govt$)|(^usps truck$)|(^usps posta$)|(^usps mail$)|(^usps #7530$)|(^usps # 487 $)|(^usps$)|(^uspos$)|(^usp m$)|(^us postal$)|(^us po$)|(^us mail tr$)|(^us ma$)|(^ups truck$)|(^ups t$)|(^ups m$)|(^ups$)|(^postal tru$)|(^postal ser$)|(^postal bus$)|(^posta$)|(^post offic$)|(^u.s. posta$)")

    let public = ($df | dataframe contains "(?i)(?i)(bus)|(cab)|(mta)|(schoo)|(shuttle)")

    let sedan = ($df | dataframe contains "(?i)(2 DOO)|(2 DR)|(2 dr sedan)|(3 doo)|(3-Door)|(2 Whe)|(2 WHE)|(3DOOR)|(4 dr sedan)|(4 RUN)|(4D)|(4DR)|(4DS)|(4dsd)|(4whee)|(Acces)|(ACCES)|(Barri)|(box)|(CHERV)|(CHEVR)|(CHEVY EXPR)|(Convertible)|(COUPE)|(cross)|(dodge)|(ELEC.)|(elect)|(ford)|(FUSION)|(Hearse)|(limo)|(Mercedes)|(yy)|(YW PO)|(YELLOWPOWE)|(Wh Ford co)|(VMS)|(vespa)|(veriz)|(VAV)|(trans)|(tlr)|(tlc)|(tf)|(SYBN)|(SWT)|(sweep)|(^sw$)|(suv)|(SUBURBAN)|(SUBUR)|(SUBR)|(SUBN - AMB)|(ST150)|(^st$)|(smart car)|(smart)|(small com veh\\(4 tires\\))|(small)|(sm yw)|(slingshot)|(skid-)|(sedona)|(sedan)|(sea)|(^se$)|(^sd$)|(scomm)|(scom)|(SANTI)|(rmb)|(RINGO)|(rgs)|(RD/S)|(range)|(priva)|(power ladd)|(power)|(POSTO)|(police rep)|(POLIC)|(perm)|(pedicab)|(pedic)|(^pc$)|(Open Body)|(OMT)|(OMR)|(OML)|(OLC)|(nyu s)|(NISSA)")

    let special = ($df | dataframe contains "(?i)(bobca)|(bobct)|(dirt)|(fork)|(forlift)|(GATOR)|(Go kart)|(gokar)|(golf)|(ice c)|(icecr)|(JOHN DEERE)|(KUBOT)|(Lift Boom)")

    let trail = ($df | dataframe contains "(?i)(^5X8 T$)|(^carri$)|(^Carry All$)|(^CART$)|(Enclosed Body)|(^G TOW$)|(^HORSE CARR$)|(^Horse Trai$)|(^trailer$)|(^TRLR$)|(^TRL$)|(^trailee$)|(^TRATLER/FL$)|(^TR-Trailer$)|(^semi trail$)|(^Motorized Home$)|(^MOTOR HOME$)|(^UTILITY TR$)|(light trai)")
    
    let van = ($df | dataframe contains "(?i)(cater)|(chassis cab)|(dept van #)|(couri)|(dodge ram)|(e450)|(econoline)|(^econo$)|(f550)|(flat)|(food)|(ford van)|(glass rack)|(hertz ram)|(jeep)|(livestock rack)|(lunch wagon)|(marked van)|(maxim)|(^mini$)|(mini bus)|(mini van)|(mini van)|(utility wh)|(utility ve)|(workh)|(work van)|(wagon)|(station wagon)|(vanette)|(van)|(utility va)|(utility)|(utili)|(utlit)|(util)|(ut)|(unmarked v)|(trali)|(street swe)|(street cle)|(stree)|(sprinter v)|(sprin)|(sport utility / station wagon)|(refg)|(refr)|(ram promas)|(ram)|(pump)|(^pu$)|(pro moaster)|(prks)|(postal veh)|(postal van)|(plow)|(pk)|(pick)|(pay loader)|(pas)|(omnib)|(omni)|(nypd)|(moving van)|(miniv)|(r/v)|(rv/tr)|(rv)")

    let unknown = ($df | dataframe contains "(?i)(G Sem)|(0)|(omme)|(1)|(11111)|(12 pa)|(12 pa)|(15 pa)|(197209)|(1s)|(2 ton)|(2 - to)|(2000)|(250-3)|(315 e)|(3dc-)|(985)|(994)|(9999)|(a-one)|(aeria)|(appor)|(app)|(black)|(boat)|(broom)|(bs)|(btm)|(c0mme)|(camp)|(cart)|(case)|(chart)|(cherr)|(con e)|(d\\/v wb)|(dent and s)|(diese)|(dot equipm)|(dot t)|(e com)|(e tow)|(east)|(ec3)|(engin)|(engine sp0)|(f15)|(free)|(department)|(g com)|(g com)|(g psd)|(g scl)|(g spc)|(ge\\/sc)|(glben)|(gover)|(government)|(government)|(gray)|(green)|(hd to)|(hi ta)|(hi-lo)|(high)|(highl)|(horse)|(horse)|(hosre)|(house)|(hrse)|(hwh)|(i-haul)|(inter)|(inter)|(inter)|(kp160)|(left)|(light)|(liver)|(livery vehicle)|(log)|(ltrl)|(mark)|(marke)|(marke)|(mcy)|(mc)|(mcy b)|(mecha)|(unkow)|(unknown)|(unkno)|(unkn)|(unk)|(ukn)|(rmp)|(none)|(na)|(n\\/a)|(n\\/a)|(vab)|(suret)|(skywatch)|(skateboard)|(skate)|(road)|(repai)|(rep)|(multi-wheeled vehicle)|(yello)|(work)|(winnie)|(white)|(whit)|(wheel)|(us)|(trans)|(te)|(sub)|(sterl)|(stak)|(special co)|(spec)|(spc)|(self)|(self insur)|(seagr)|(sc)|(sanmen cou)|(rood)|(renta)|(red t)|(psp)|(psh)|(psd)|(ps)|(pois)|(pet)|(p\\/sh)|(p\\/sh)|(oz mo)|(other)|(177omm)|(oil t)|(nycta)|(nycha)|(nyc d)|(nyc a)|(nttrl)|(ns am)|(not i)|(nonmotords)|(no\\/bu)|(new y)|(nat grid t)|(mtr s)|(movin)|(city)|(city)")

    
    let plain_truck = ($df | dataframe contains "truck")

    let unknown2 = ($plain_truck ||
        $truck ||
        $van ||
        $sedan||
        $special ||
        $trail ||
        $bike ||
        $unknown ||
        $motor ||
        $commercial ||
        $amb)

    let unknown2 = ($unknown2 | dataframe not)

    


    let new = ($df | 
        dataframe set "Truck" --mask $plain_truck | 
        dataframe set "Unknown" --mask $unknown |
        dataframe set "Van" --mask $van |
        dataframe set "Sedan" --mask $sedan |
        dataframe set "Special" --mask $special |
        dataframe set "Commercial" --mask $commercial |
        dataframe set "Bus" --mask $public |
        dataframe set "Bike" --mask $bike |
        dataframe set "Motor" --mask $motor |
        dataframe set "Ambulance" --mask $amb |
        dataframe set "Trail" --mask $trail |
        dataframe set "Truck" --mask $truck |
        dataframe set "Unknown" --mask $unknown2)

    $df_origin | dataframe column "COLLISION_ID" | dataframe with-column $new --name  "type" | dataframe to-csv $"truck_type-($name).csv"

}