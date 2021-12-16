match_vehicle <- function(originVehicle) {
    require(stringi)
    require(tidyverse)

    truck_match <- str_remove_all("(?i)
(18 wh)|
(18 wheeler)|
(26ft)|
(35 ft)|
(4 axe)|
(armor)|
(armored truck)|
(back)|
(backh)|
(beverage truck)|
(boom)|
(box t)|
(boxtr)|
(bucke)|
(bulk agriculture)|
(bulld)|
(cargo)|
(catapillar)|
(cemen)|
(cm)|
(comix)|
(con ed tru)|
(concrete mixer)|
(cat)|
(const)|
(cont)|
(crane)|
(delivery t)|
(dhl t)|
(dilevery t)|
(drill rig)|
(dp)|
(dump)|
(e one)|
(escavator)|
(excav)|
(farm vehicle)|
(freightlin)|
(fr`)|
(freig)|
(frht)|
(frieg)|
(front-load)|
(front)|
(fuel)|
(garba)|
(grain)|
(heavy)|
(hino)|
(hopper)|
(kenwo)|
(large com veh(6 or more tires))|
(liebh)|
(mac t)|
(mack)|
(work truck)|
(well driller)|
(uhual)|
(uhal)|
(u-hau)|
(u_hal)|
(u haul tru)|
(u hau)|
(trk)|
(tram)|
(tract)|
(trac.)|
(trac)|
(tr)|
(toter)|
(tow)|
(tl)|
(tk)|
(tir)|
(tank)|
(stake)|
(snow)|
(skid loade)|
(shove)|
(sgws)|
(semi)|
(sanit)|
(road sweep)|
(rescu)|
(quality tr)|
(power shov)|
(parce)|
(nyc sanita)|
(nt tr)|
(moving tru)", "\n")

    public_match <- str_remove_all("(?i)
(?i)(bus)|
(cab)|
(mta)|
(schoo)|
(shuttle)", "\n")

    commercial_match <- str_remove_all("(?i)
(^co$)|
(^com$)|
(^com v$)|
(^comer$)|
(^comm$)|
(^commercial$)|
(^commerical$)|
(^fleet$)|
(^commm$)|
(^commu$)|
(^cab$)|
(^cabin$)|
(^deiv$)|
(^del$)|
(^delivery$)|
(delv)|
(^fed e$)|
(^fedex$)|
(^fedx$)|
(^grumm)|
(^mail$)|
(^mail truck$)|
(^taxi$)|
(^usps2$)|
(^usps/govt$)|
(^usps truck$)|
(^usps posta$)|
(^usps mail$)|
(^usps #7530$)|
(^usps # 487 $)|
(^usps$)|
(^uspos$)|
(^usp m$)|
(^us postal$)|
(^us po$)|
(^us mail tr$)|
(^us ma$)|
(^ups truck$)|
(^ups t$)|
(^ups m$)|
(^ups$)|
(^postal tru$)|
(^postal ser$)|
(^postal bus$)|
(^posta$)|
(^post offic$)|
(^u.s. posta$)
", "\n")

    amb_match <- str_remove_all("(?i)
(ABULA)|
(^amb$)|
(ambu)|
(amulance)|
(E.M.S)|
(EMERGENCY)|
(EMRGN)|
(EMS)|
(fdny)|
(GEN  AMBUL)|
(G AMB)|
(white ambu)|
(NYS AMBULA)|
(NYS A)|
(nyc EMS)|
(Fd fi)|
(fd tr)|
(FD tr)|
(FD TR)|
(fire)|
(FIRER)|
(firet)|
(FIRTRUCK)|
(KME/F)|
(ladde)|
(LADDER)|
(nyfd)|
(NYC FD)|
(NYC F)", "\n")

    van_match <- str_remove_all("(?i)
(cater)|
(chassis cab)|
(dept van #)|
(couri)|
(dodge ram)|
(e450)|
(econoline)|
(^econo$)|
(f550)|
(flat)|
(food)|
(ford van)|
(glass rack)|
(hertz ram)|
(jeep)|
(livestock rack)|
(lunch wagon)|
(marked van)|
(maxim)|
(^mini$)|
(mini bus)|
(mini van)|
(mini van)|
(utility wh)|
(utility ve)|
(workh)|
(work van)|
(wagon)|
(station wagon)|
(vanette)|
(van)|
(utility va)|
(utility)|
(utili)|
(utlit)|
(util)|
(ut)|
(unmarked v)|
(trali)|
(street swe)|
(street cle)|
(stree)|
(sprinter v)|
(sprin)|
(sport utility / station wagon)|
(refg)|
(refr)|
(ram promas)|
(ram)|
(pump)|
(^pu$)|
(pro moaster)|
(prks)|
(postal veh)|
(postal van)|
(plow)|
(pk)|
(pick)|
(pay loader)|
(pas)|
(omnib)|
(omni)|
(nypd)|
(moving van)|
(miniv)|
(r/v)|
(rv/tr)|
(rv)", "\n")

    special_match <- str_remove_all("(?i)
(bobca)|
(bobct)|
(dirt)|
(fork)|
(forlift)|
(GATOR)|
(Go kart)|
(gokar)|
(golf)|
(ice c)|
(icecr)|
(JOHN DEERE)|
(KUBOT)|
(Lift Boom)
", "\n")
    sedan_match <- str_remove_all("(?i)
(2 DOO)|
(2 DR)|
(2 dr sedan)|
(3 doo)|
(3-Door)|
(2 Whe)|
(2 WHE)|
(3DOOR)|
(4 dr sedan)|
(4 RUN)|
(4D)|
(4DR)|
(4DS)|
(4dsd)|
(4whee)|
(Acces *)|
(ACCES *)|
(Barri)|
(box)|
(CHERV)|
(CHEVR)|
(CHEVY EXPR)|
(Convertible * (fast))|
(COUPE)|
(cross)|
(dodge)|
(ELEC.)|
(elect)|
(ford)|
(FUSION)|
(Hearse)|
(limo)|
(Mercedes)|
(yy)|
(YW PO)|
(YELLOWPOWE)|
(Wh Ford co)|
(VMS)|
(vespa)|
(veriz)|
(VAV)|
(trans)|
(tlr)|
(tlc)|
(tf)|
(SYBN)|
(SWT)|
(sweep)|
(^sw$)|
(suv)|
(SUBURBAN)|
(SUBUR)|
(SUBR)|
(SUBN - AMB)|
(ST150)|
(^st$)|
(smart car)|
(smart)|
(small com veh\\(4 tires\\))|
(small)|
(sm yw)|
(slingshot)|
(skid-)|
(sedona)|
(sedan)|
(sea)|
(^se$)|
(^sd$)|
(scomm)|
(scom)|
(SANTI)|
(rmb)|
(RINGO)|
(rgs)|
(RD/S)|
(range)|
(priva)|
(power ladd)|
(power)|
(POSTO)|
(police rep)|
(POLIC)|
(perm)|
(pedicab)|
(pedic)|
(^pc$)|
(Open Body)|
(OMT)|
(OMR)|
(OML)|
(OLC)|
(nyu s)|
(NISSA)", "\n")

    trail_match <- str_remove_all("(?i)
(^5X8 T$)|
(^carri$)|
(^Carry All$)|
(^CART$)|
(Enclosed Body)|
(^G TOW$)|
(^HORSE CARR$)|
(^Horse Trai$)|
(^trailer$)|
(^TRLR$)|
(^TRL$)|
(^trailee$)|
(^TRATLER/FL$)|
(^TR-Trailer$)|
(^semi trail$)|
(^Motorized Home$)|
(^MOTOR HOME$)|
(^UTILITY TR$)", "\n")

    motor_match <- str_remove_all("(?i)
(50cc Scoot)|
(atv p (heavy))|
(gas mo ped)|
(GAS POWERE)|
(gas scoote)|
(Gas scoote)|
(Yamaha)|
(SPARK150 S)|
(servi)|
(Pavin)|
(motorscooter)|
(motorscoot)|
(motorcycle)|
(motorbike)|
(motor scoo)|
(motor)|
(moped)|
(mopd)|
(mopad)|
(MO PE)|
(Mo pa)|
(Minicycle)|
(Minibike)", "\n")

    bike_match <- str_remove_all("(?i)
(2 WHE)|
(Bicyc)|
(BICYCLE)|
(Bike)|
(BK)|
(E BIK)|
(E bike)|
(E REVEL SC)|
(e sco)|
(E SCO)|
(E scooter)|
(e-bik)|
(E-Bik)|
(E-bike)|
(E-Bike)|
(E-BIKE)|
(E-MOT)|
(E-Sco)|
(E-scooter	)|
(E-Scooter)|
(E-UNICYCLE)|
(ebike)|
(Ebike)|
(EBIKE)|
(ELECTRIC B)|
(ELEC. UNIC)|
(kick scoot)|
(segwa)|
(scooter)|
(scooter GA)|
(scoot)|
(scoo)|
(PUSH SCOOT)|
(pallet)", "\n")

    unknown_match <- str_remove_all("(?i)
(0)|
(?omme)|
(1)|
(11111)|
(12 Pa)|
(12 PA)|
(15 Pa)|
(197209)|
(1S)|
(2 TON)|
(2 - to)|
(2000)|
(250-3)|
(315 e)|
(3DC-)|
(985)|
(994)|
(9999)|
(A-ONE)|
(AERIA)|
(APPOR)|
(APP)|
(BLACK)|
(BOAT)|
(BROOM)|
(BS)|
(BTM)|
(C0MME)|
(CAMP)|
(CART)|
(CASE)|
(CHART)|
(CHERR)|
(CON E)|
(D/V WB)|
(Dent and S)|
(DIESE)|
(DOT EQUIPM)|
(DOT T)|
(e com)|
(e tow)|
(east)|
(EC3)|
(ENGIN)|
(ENGINE SP0)|
(f15)|
(FREE)|
(DEPARTMENT)|
(G com)|
(G COM)|
(G PSD)|
(g scl)|
(g spc)|
(GE/SC)|
(GLBEN)|
(GOVER)|
(government)|
(Government)|
(GRAY)|
(GREEN)|
(HD To)|
(HI TA)|
(hi-lo)|
(high)|
(HIGHL)|
(Horse)|
(HORSE)|
(HOSRE)|
(HOUSE)|
(Hrse)|
(HWH)|
(I-haul)|
(inter)|
(Inter)|
(INTER)|
(KP160)|
(LEFT)|
(LIGHT TRAI	)|
(LIGHT)|
(LIVER)|
(LIVERY VEHICLE)|
(Log)|
(LTRL)|
(MARK)|
(Marke	)|
(marke)|
(mcy	)|
(mc)|
(MCY B)|
(Mecha)|
(UNKOW)|
(UNKNOWN)|
(UNKNO)|
(UNKN)|
(UNK)|
(ukn)|
(rmp)|
(none)|
(na)|
(N/A)|
(n/a)|
(VAB)|
(SURET)|
(skywatch)|
(skateboard)|
(skate)|
(road)|
(REPAI)|
(REP)|
(Multi-Wheeled Vehicle)|
(YELLO)|
(work)|
(winnie)|
(white)|
(whit)|
(wheel)|
(US)|
(trans)|
(te)|
(SUB)|
(sterl)|
(stak)|
(special co)|
(spec)|
(spc)|
(self)|
(self INSUR)|
(seagr)|
(SC)|
(SANMEN COU)|
(rood)|
(RENTA)|
(red t)|
(PSP)|
(PSH)|
(PSD)|
(PS)|
(pois)|
(pet)|
(p/sh)|
(P/SH)|
(OZ MO)|
(OTHER)|
(\177omm)|
(OIL T)|
(nycta)|
(nycha)|
(NYC D)|
(NYC a)|
(NTTRL)|
(NS AM)|
(NOT I)|
(NonMotordS)|
(NO/BU)|
(NEW Y)|
(nat grid t)|
(mtr s)|
(MOVIN)|
(City)|
(CITY)", "\n")


    later_truck_match <-
        "(?i)(truck)"

    case_when(
        stri_detect_regex(originVehicle, truck_match) ~ "Truck",
        stri_detect_regex(originVehicle, public_match) ~ "Public",
        stri_detect_regex(originVehicle, commercial_match) ~ "Commercial",
        stri_detect_regex(originVehicle, amb_match) ~ "Ambulance",
        stri_detect_regex(originVehicle, van_match) ~ "Van",
        stri_detect_regex(originVehicle, special_match) ~ "Special",
        stri_detect_regex(originVehicle, sedan_match) ~ "Sedan",
        stri_detect_regex(originVehicle, motor_match) ~ "Motor",
        stri_detect_regex(originVehicle, bike_match) ~ "Bike",
        stri_detect_regex(originVehicle, later_truck_match) ~ "Truck",
        stri_detect_regex(originVehicle, unknown_match) ~ "Unknown",
        T ~ "Unexpected"
    )
}

match_vehicle("sdf")