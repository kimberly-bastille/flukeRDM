
cd $input_data_cd


*This code only needs to be run once after new MRIP data enters the repo

foreach wave in	$yr_wvs {				

capture confirm file "trip_`wave'.dta"
if _rc==0{
	use trip_`wave'.dta, clear
	renvarlab, lower
	save trip_`wave'.dta, replace
}

else{
	
}

capture confirm file "size_b2_`wave'.dta"
if _rc==0{
	use size_b2_`wave'.dta, clear
	renvarlab, lower
	save size_b2_`wave'.dta, replace
}

else{
	
}

capture confirm file "size_`wave'.dta"
if _rc==0{
	use size_`wave'.dta, clear
	renvarlab, lower
	save size_`wave'.dta, replace
}

else{
	
}

capture confirm file "catch_`wave'.dta"
if _rc==0{
	use catch_`wave'.dta, clear
	renvarlab, lower
	save catch_`wave'.dta, replace
}

else{
	
}

}


/*catchlist -- this assembles then names of files that are needed in the catchlist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global catchlist
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "catch_`year'`wave'.dta"
	if _rc==0{
		use "catch_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global catchlist "$catchlist "catch_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*Triplist -- this assembles then names of files that are needed in the Triplist */
/*Check to see if the file exists */	/* If the file exists, add the filename to the list if there are observations */
global triplist
foreach year in $yearlist{
	foreach wave in  $wavelist{
	capture confirm file "trip_`year'`wave'.dta"
	if _rc==0{
		use "trip_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global triplist "$triplist "trip_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}

/*B2 Files*/
global b2list
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "size_b2_`year'`wave'.dta"
	if _rc==0{
		use "size_b2_`year'`wave'.dta", clear
		quietly count
		scalar tt=r(N)
		if scalar(tt)>0{
			global b2list "$b2list "size_b2_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}


/*SIZE_LIST */
global sizelist
foreach year in $yearlist{
	foreach wave in $wavelist{
	capture confirm file "size_`year'`wave'.dta"
	if _rc==0{
	use "size_`year'`wave'.dta", clear
	quietly count
	scalar tt=r(N)
	if scalar(tt)>0{
		global sizelist "$sizelist "size_`year'`wave'.dta " " 
		}
		else{
		}
	}
	else{
	}
	
}
}

