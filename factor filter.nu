let vehicle = (dataframe open Motor_Vehicle_Collisions_-_Crashes.csv)

filterFactor $vehicle "1"
filterFactor $vehicle "2"
filterFactor $vehicle "3"
filterFactor $vehicle "4"
filterFactor $vehicle "5"

def filterFactor [df_origin, name] {
    let $df = ($df_origin | dataframe column $"CONTRIBUTING FACTOR VEHICLE ($name)")

    let fatigue =  ($df | 
        dataframe contains "(?i)(fatigued/drowsy)|(fell asleep)")

    let pedestrian =  ($df | 
        dataframe contains "(?i)(pedestrian)|(bicycle)|(motorcycle)|(pedal)|(cycle)")

    let defection = ($df | 
        dataframe contains "(?i)(defective)|(oversized vehicle)|(steering failure)|(other lighting defects)|(headlights defective)|(tinted windows)|(tire failure)|(windshield inadequate)")

    let drug = ($df |
        dataframe contains "(?i)(alcohol)|(drug)"
        )

    let rule = ($df | 
        dataframe contains "(?i)(failure to yield right)|(passing or lane usage improper)|(turning improperly)|(unsafe lane changing)|(traffic control disregarded)|(unsafe speed)|(aggressive driving)|(failure to keep right)|(aggressive driving/road rage)")

    let distraction = ($df | dataframe contains "(?i)(driver inattention\\/distraction)|(following too closely)|(backing unsafely)|(passing too closely)|(outside car distraction)|(distraction)|(eating or drinking)|(listening\\/using headphones)|(using on board navigation device)|(Electronic)|(Cell Phone.*hands-free)|(Headphones)")

    let phone = ($df | dataframe contains "(?i)(Cell Phone.*hand-held)|(texting)")

    let environment = ($df | 
        dataframe contains "(?i)(other vehicular)|(pavement slippery)|(reaction to uninvolved vehicle)|(pedestrian/bicyclist)|(glare)|(obstruction/debris)|(pavement defective)|(reaction to other uninvolved vehicle)|(animals action)|(lane marking improper)|(traffic control device improper)|(driverless/runaway vehicle)|(Vandalism)")
    
    let health = ($df | dataframe contains "(?i)(lost consciousness)|(physical disability)|(illness)|(illnes)|(shoulder defective)")

    let majeure = ($df | 
        dataframe contains "(?i)(prescription medication)|(driver inexperience)|(view obstructed)")

    let new = ($df |
        dataframe set "Fatigue" --mask $fatigue |
        dataframe set "Pedestrian" --mask $pedestrian |
        dataframe set "V Defection" --mask $defection |
        dataframe set "Drug" --mask $drug |
        dataframe set "Disobey Rule" --mask $rule |
        dataframe set "Distraction" --mask $distraction |
        dataframe set "Environment" --mask $environment |
        dataframe set "Phone Held" --mask $phone |
        dataframe set "Health" --mask $health |
        dataframe set "Majeure" --mask $majeure)
        
    $new | dataframe to-csv $"factor-($name).csv"
}