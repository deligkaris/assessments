#! /usr/bin/perl -w 

use diagnostics;
use strict;

#*********************************************************************************************************************************************************
##
## Author: Christos Deligkaris
##
## Date: August 2014
##
## Purpose: this program reads the student responses (pre- and post- if both are available), analyzes them and produces some statistical results
##
## Usage: simply execute this perl script. need to have a folder "DATA-PRE" and/or "DATA-POST" in the same directory with this script.	
##	  also need to have the following files: 	choices (options of multiple choice questions)
##							x_solutions (key to the questions)
##							included (which questions to grade)
##        all student responses have to be in the appropriate rows, ie row 1 is the answer to question 1 etc
##		
## ********************************************************************************************************************************************************

#DEFINITIONS
my (@myfolders,$folder,@my_post_files,@my_pre_files,$file,$line);
my ($flag_pre,$flag_post);  #FLAGS FOR HAVING PRE AND POST DATA
my ($i,$j,$k);	#USED IN LOOPS
my (%post_data,%pre_data,@included_problems,@solutions,@choices,@choice_matrix_pre,@choice_matrix_post);
my ($nproblems,$nincluded,$nstudents,%ncorrect_pre,%ncorrect_post,$ncorrect_total_pre,$ncorrect_total_post,$ncorrect_mean_pre,$ncorrect_mean_post,$ncorrect_min_pre,$ncorrect_min_post,$ncorrect_max_pre,$ncorrect_max_post);
my (@ncorrect_sorted_pre,@ncorrect_sorted_post,$nchoices); #SORTED PRE AND POST DATA
my ($nstudents_gain); 
my ($gain_norm_total,%gain_norm,$gain_norm_mean,$gain_norm_min,$gain_norm_max,@gain_norm_sorted); #NORMALIZED GAIN
my ($gain_total,%gain,$gain_mean,$gain_min,$gain_max,@gain_sorted); #RAW GAIN
my ($score_pre_total,$score_post_total,$score_pre_mean,$score_post_mean,$gain_mean_hake,$gain_norm_mean_hake); #HAKE's METHOD

if( ! open LOGFILE, ">", "logfile" ) {
	die "CANNOT CREATE/OPEN LOGFILE";
}

printf LOGFILE "*********" x 13;
printf LOGFILE "\n\nTUGK REPORT\n\n";
printf LOGFILE "*********" x 13;
printf LOGFILE "\n\n";
printf LOGFILE "*********" x 13;
printf LOGFILE "\nSECTION 1: READING SURVEY DATA\n";
printf LOGFILE "*********" x 13;
printf LOGFILE "\n\n";

#read the possible choices for the multiple choice questions
if( ! open CHOICES, "<","tugk_choices") {
	die "CANNOT READ FILE choices";
}

$i=0;
while ($line = <CHOICES>) {
	$i=$i+1;
	$choices[$i]=substr($line,0,1);
}
printf LOGFILE "READ CHOICES FROM FILE...";

#find how many different choices there are for the multiple choice questions
$nchoices = $#choices;
printf LOGFILE "THERE ARE %g CHOICES FOR THE MULTIPLE CHOICE QUESTIONS\n",$nchoices;

@myfolders=glob("DATA-*");	#find all the DATA folders in the current directory
#print @myfolders

#flags for pre-test and post-test data, default is no data for both pre- and post-tests
$flag_pre = 0;
$flag_post = 0;

#determine whether we have pre, post data or both
foreach $folder (@myfolders) {
	if($folder =~ m/DATA-PRE/) {
		$flag_pre = 1;
		printf LOGFILE "FOUND DATA FOR PRE-TEST...";
	}
	elsif($folder =~ m/DATA-POST/) {
                $flag_post = 1;
		printf LOGFILE "FOUND DATA FOR POST-TEST...";
        }
}
if($flag_pre == 0 && $flag_post == 0) {
	printf LOGFILE "\nDID NOT FIND ANY DATA...NOTHING TO DO :(\n\n";
}

#read the solutions from a text file in the current directory
if( ! open SOLUTIONS, "<", "tugk_solutions") {
	die "CANNOT READ FILE tugk_solutions";
}
printf LOGFILE "\nOPENED KEY....";
$i=0;
while ($line = <SOLUTIONS>) {
	$i=$i+1;
	$solutions[$i]=substr($line,0,1);
}
printf LOGFILE "FINISHED READING ANSWER KEY...";
$nproblems=$i;  
printf LOGFILE "THERE ARE %s PROBLEMS IN THIS ASSESSMENT TOOL",$nproblems;

if( ! open INCLUDED, "<", "included") {
	die "CANNOT READ FILE included";
}
printf LOGFILE "\nOPENED INCLUDED PROBLEMS...";
$i=0;
$j=0;
while ($line = <INCLUDED>) {
	$i=$i+1;
	if (substr($line,0,1) =~ m/Y/) { 
		$j=$j+1;
		$included_problems[$j]=$i;
	}
}
$nincluded=$j;
printf LOGFILE "FINISHED READING INCLUDED PROBLEMS...";
printf LOGFILE "THERE ARE %s PROBLEMS INCLUDED IN THESE CALCULATIONS\n\n",$nincluded;
printf LOGFILE "*********" x 13;
printf LOGFILE "\nEND OF SECTION 1: READING SURVEY DATA\n";
printf LOGFILE "*********" x 13;
printf LOGFILE "\n\n";

#do the analysis of the data
if($flag_pre == 0) { #no data for pre-test

	if($flag_post == 1) { #data for post-test exist

                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nSECTION 2: READING AND ANALYZING POST-TEST DATA\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";
		printf LOGFILE "ANALYZING POST-TEST DATA\n";
		chdir "./DATA-POST" or die "\nCANNOT CHANGE DIRECTORY TO POST-DATA\n\n";
		printf LOGFILE "READING STUDENT ANSWERS....";
		@my_post_files=glob("student-*");
		$nstudents=0;
		foreach $file (@my_post_files) {
			$i=substr($file,8,2); #get the student number, a string but strings automatically convert to numbers in perl
			if ( ! open ANSWERS, "<", "$file") {
				die "CANNOT OPEN STUDENT ANSWER FILES";
			}
			$j=0;
			while ($line = <ANSWERS>) {
				$j=$j+1;
				if ( $j ~~ @included_problems) {
					$post_data{$i}{$j} = substr($line,0,1);
				}
			}
			$nstudents=$nstudents+1;
		}
		printf LOGFILE "FOUND DATA FOR %s STUDENTS....DONE READING STUDENT DATA\n",$nstudents;
		chdir "../" or die "\nCANNOT CHANGE DIRECTORY\n\n";
		printf LOGFILE "DOING ANALYSIS OF DATA...";

		#find how many correct answers each student has
		foreach $i (sort keys %post_data) {
			$k=0;
			for ($j=1;$j<=$nproblems;$j++) {
				if ($j ~~ @included_problems ) {
					if ($post_data{$i}{$j} eq $solutions[$j]) {
						$k=$k+1;
					}
				}
			}
			$ncorrect_post{$i}=$k;
		}
		#do the statistics, mean of correct answers, minimum, and maximum of correct answers
		$ncorrect_total_post=0;
		foreach $i (sort keys %post_data) {
                        $ncorrect_total_post=$ncorrect_total_post+$ncorrect_post{$i};
                }
                $ncorrect_mean_post=$ncorrect_total_post/($nstudents);
                @ncorrect_sorted_post = sort {$a <=> $b} (values %ncorrect_post); #numerical sorting (not done by default!)
                $ncorrect_min_post= $ncorrect_sorted_post[0];
                $ncorrect_max_post= $ncorrect_sorted_post[-1];

		#analyze in detail how many students chose each multiple choice option
		for ($i=1;$i<=$nproblems;$i++) {
			if( $i ~~ @included_problems ) {
				for ($j=1;$j<=$nchoices;$j++) {
					$choice_matrix_post[$i][$j]=0;
					foreach $k (sort keys %post_data) {
						if ($post_data{$k}{$i} eq $choices[$j]) {
							$choice_matrix_post[$i][$j]=$choice_matrix_post[$i][$j]+1;
						}
					}
				}
			}
		}
		printf LOGFILE "ANALYSIS IS COMPLETED\n\n";
		printf LOGFILE "*********" x 13;
		printf LOGFILE "\nEND OF SECTION 2: READING AND ANALYZING POST-TEST DATA\n";
		printf LOGFILE "*********" x 13;
		printf LOGFILE "\n\n";

		#report results to the logfile
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nSECTION 3: REPORT OF POST-TEST RESULTS\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";

		printf LOGFILE "RESULTS PER STUDENT:\n\n";
		printf LOGFILE "\nSTUDENT  ** CORRECT ANSWERS ** CORRECT ANSWERS (PERCENT) ** (out of %s problems)\n", $nincluded;
		foreach $i (sort keys %post_data) {
                        printf LOGFILE "%7g     %15g     %15.2g\n",$i,$ncorrect_post{$i},100*$ncorrect_post{$i}/$nincluded;
		}
		printf LOGFILE "\n\n";
		
		printf LOGFILE "STATISTICS:\n\n";
		printf LOGFILE "AVERAGE CORRECT QUESTIONS: %4.2g, MINIMUM CORRECT QUESTIONS: %4.2g, MAXIMUM CORRECT QUESTIONS: %4.2g\n\n",$ncorrect_mean_post,$ncorrect_min_post,$ncorrect_max_post;
		printf LOGFILE "AVERAGE CORRECT PERCENT: %4.2g, MINIMUM CORRECT PERCENT: %4.2g, MAXIMUM CORRECT PERCENT: %4.2g\n\n",100.*$ncorrect_mean_post/$nincluded,100.*$ncorrect_min_post/$nincluded,100.*$ncorrect_max_post/$nincluded;

		printf LOGFILE "\n\n";
		printf LOGFILE "DETAILED ANALYSIS OF FREQUENCY OF STUDENT CHOICES PER PROBLEM:\n\n";	
		printf LOGFILE "PROBLEM ";
		for ($i=1;$i<=$nchoices;$i++) {
			printf LOGFILE "** %s ",$choices[$i];
		}
		printf LOGFILE "\n\n";
                for ($i=1;$i<=$nproblems;$i++) {
			if ( $i ~~ @included_problems ) {
				printf LOGFILE "%7g",$i;
                        	for ($j=1;$j<=$nchoices;$j++) {
					printf LOGFILE "%5g",$choice_matrix_post[$i][$j];
                        	}
				printf LOGFILE "\n";
			}
                }

		printf LOGFILE "\n\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nEND OF SECTION 3: REPORT OF POST-TEST RESULTS\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";

		printf LOGFILE "\n\nEND OF LOGFILE";	
	}
	else {
		printf "\nERROR: NOTHING TO DO!\n\n";
	}
} #end of no pre-test data
elsif($flag_pre == 1) { #if pre-test data exist

                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nSECTION 2: READING AND ANALYZING PRE-TEST DATA\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";
                printf LOGFILE "ANALYZING PRE-TEST DATA \n";
                chdir "./DATA-PRE" or die "\nCANNOT CHANGE DIRECTORY TO PRE-DATA\n\n";
                printf LOGFILE "READING STUDENT ANSWERS....";
                @my_pre_files=glob("student-*");
                $nstudents=0;
                foreach $file (@my_pre_files) {
			$i=substr($file,8,2); #get the student number, a string but strings automatically convert to numbers in perl
                        if ( ! open ANSWERS, "<", "$file") {
				die "CANNOT OPEN STUDENT ANSWER FILE";
			}
                        $j=0;
                        while ($line = <ANSWERS>) {
				$j=$j+1;
                                	if ( $j ~~ @included_problems) {
						$pre_data{$i}{$j} = substr($line,0,1); #get students response
					}
                        }
			$nstudents=$nstudents+1; #found results for one more student!
                }
                printf LOGFILE "FOUND PRE-TEST DATA FOR %s STUDENTS....DONE READING STUDENT PRE-TEST DATA\n",$nstudents;
                chdir "../" or die "\nCANNOT CHANGE DIRECTORY\n\n";
                printf LOGFILE "DOING ANALYSIS OF DATA...";

		#find how many correct answers each student has
                foreach $i (sort keys %pre_data) { 
                        $k=0;
                        for ($j=1;$j<=$nproblems;$j++) {
				if ( $j ~~ @included_problems) {
                                	if ($pre_data{$i}{$j} eq $solutions[$j]) {
                                        	$k=$k+1;
					}
                                }
                        }
                        $ncorrect_pre{$i}=$k;
               	}

                #do the statistics, mean of correct answers, minimum, and maximum of correct answers
                $ncorrect_total_pre=0;
                foreach $i (sort keys %pre_data) {
                        $ncorrect_total_pre=$ncorrect_total_pre+$ncorrect_pre{$i};
                }
                $ncorrect_mean_pre=$ncorrect_total_pre/($nstudents);
                @ncorrect_sorted_pre = sort {$a <=> $b} (values %ncorrect_pre); #numerical sorting (not done by default!)
                $ncorrect_min_pre= $ncorrect_sorted_pre[0];
                $ncorrect_max_pre= $ncorrect_sorted_pre[-1];

                #analyze in detail how many students chose each multiple choice option
                for ($i=1;$i<=$nproblems;$i++) {
			if ( $i ~~ @included_problems) {
                        	for ($j=1;$j<=$nchoices;$j++) {
                                	$choice_matrix_pre[$i][$j]=0;
                                	foreach $k (sort keys %pre_data) {
                                        	if ($pre_data{$k}{$i} eq $choices[$j]) {
                                                	$choice_matrix_pre[$i][$j]=$choice_matrix_pre[$i][$j]+1;
                                        	}
                                	}
                        	}
			}
                }
                printf LOGFILE "ANALYSIS IS COMPLETED\n\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nEND OF SECTION 2: READING AND ANALYZING PRE-TEST DATA\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";

                #report results to the logfile
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nSECTION 3: REPORT OF PRE-TEST RESULTS\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";

                printf LOGFILE "RESULTS PER STUDENT:\n\n";
                printf LOGFILE "\nSTUDENT  ** CORRECT ANSWERS ** CORRECT ANSWERS (PERCENT) ** (out of %s problems)\n", $nincluded;
                foreach $i (sort keys %pre_data) {
                        printf LOGFILE "%7g     %15g     %15.2g\n",$i,$ncorrect_pre{$i},100*$ncorrect_pre{$i}/$nincluded;
                }
                printf LOGFILE "\n\n";

                printf LOGFILE "STATISTICS:\n\n";
                printf LOGFILE "AVERAGE CORRECT QUESTIONS: %4.2g, MINIMUM CORRECT QUESTIONS: %4.2g, MAXIMUM CORRECT QUESTIONS: %4.2g\n\n",$ncorrect_mean_pre,$ncorrect_min_pre,$ncorrect_max_pre;
                printf LOGFILE "AVERAGE CORRECT PERCENT: %4.2g, MINIMUM CORRECT PERCENT: %4.2g, MAXIMUM CORRECT PERCENT: %4.2g\n\n",100.*$ncorrect_mean_pre/$nincluded,100.*$ncorrect_min_pre/$nincluded,100.*$ncorrect_max_pre/$nincluded;

                printf LOGFILE "\n\n";
                printf LOGFILE "DETAILED ANALYSIS OF FREQUENCY OF STUDENT CHOICES PER PROBLEM:\n\n";
                printf LOGFILE "PROBLEM ";
                for ($i=1;$i<=$nchoices;$i++) {
                        printf LOGFILE "** %s ",$choices[$i];
                }
                printf LOGFILE "\n\n";
                for ($i=1;$i<=$nproblems;$i++) {
			if ( $i ~~ @included_problems) {
                        	printf LOGFILE "%7g",$i;
                        	for ($j=1;$j<=$nchoices;$j++) {
                                	printf LOGFILE "%5g",$choice_matrix_pre[$i][$j];
                        	}
                        	printf LOGFILE "\n";
			}
                }

                printf LOGFILE "\n\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nEND OF SECTION 3: REPORT OF PRE-TEST RESULTS\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";

         if($flag_post == 0) { #if no post-data exist

		printf LOGFILE "*********" x 13;
		printf LOGFILE "\nSECTION 4: NOTHING TO DO, NO POST-DATA ARE AVAILABLE\n";
		printf LOGFILE "*********" x 13;
		printf LOGFILE "\n\nEND OF LOGFILE";

        }
        elsif($flag_post == 1) { #if post-data do exist

                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nSECTION 4: READING AND ANALYZING POST-TEST DATA\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";
                printf LOGFILE "ANALYZING POST-TEST DATA \n";
                chdir "./DATA-POST" or die "\nCANNOT CHANGE DIRECTORY TO POST-DATA\n\n";
                printf LOGFILE "READING STUDENT ANSWERS....";
                @my_post_files=glob("student-*");
		$nstudents=0; #initialize the number of students
                foreach $file (@my_post_files) {
			$i=substr($file,8,2); #get the student number, a string but strings automatically convert to numbers in perl
                        if ( ! open ANSWERS, "<", "$file") {
				die "CANNOT READ STUDENT ANSWER FILE";
			}
                        $j=0;
                        while ($line = <ANSWERS>) {
                                $j=$j+1;
				if ( $j ~~ @included_problems) {
	                                $post_data{$i}{$j} = substr($line,0,1);
        	                }
			}
			$nstudents=$nstudents+1;
                }
                printf LOGFILE "FOUND POST DATA FOR %s STUDENTS....DONE READING STUDENT POST-TEST DATA\n",$nstudents;
                chdir "../" or die "\nCANNOT CHANGE DIRECTORY\n\n";
                printf LOGFILE "DOING ANALYSIS OF POST-TEST DATA...";

                #find how many correct answers each student has
                foreach $i (sort keys %post_data) {
                        $k=0;
                        for ($j=1;$j<=$nproblems;$j++) {
				if ( $j ~~ @included_problems) {
	                                if ($post_data{$i}{$j} eq $solutions[$j]) {
        	                                $k=$k+1;
                	                }
				}
                        }
                        $ncorrect_post{$i}=$k;
               }
                #do the statistics, mean of correct answers, minimum, and maximum of correct answers
                $ncorrect_total_post=0;
		foreach $i (sort keys %post_data) {
                        $ncorrect_total_post=$ncorrect_total_post+$ncorrect_post{$i};
                }
                $ncorrect_mean_post=$ncorrect_total_post/($nstudents);
                @ncorrect_sorted_post = sort {$a <=> $b} (values %ncorrect_post); #numerical sorting (not done by default!)
                $ncorrect_min_post= $ncorrect_sorted_post[0];
                $ncorrect_max_post= $ncorrect_sorted_post[-1];

                #analyze in detail how many students chose each multiple choice option
                for ($i=1;$i<=$nproblems;$i++) {
			if ( $i ~~ @included_problems) {
	                        for ($j=1;$j<=$nchoices;$j++) {
        	                        $choice_matrix_post[$i][$j]=0;
					foreach $k (sort keys %post_data) {
                        	                if ($post_data{$k}{$i} eq $choices[$j]) {
                                	                $choice_matrix_post[$i][$j]=$choice_matrix_post[$i][$j]+1;
                                        	}
                                	}
                        	}
			}
                }
                printf LOGFILE "ANALYSIS IS COMPLETED\n\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nEND OF SECTION 4: READING AND ANALYZING POST-TEST DATA\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";

                #report results to the logfile
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nSECTION 5: REPORT OF POST-TEST RESULTS\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";

                printf LOGFILE "RESULTS PER STUDENT:\n\n";
                printf LOGFILE "\nSTUDENT  ** CORRECT ANSWERS ** CORRECT ANSWERS (PERCENT) ** (out of %s problems)\n", $nincluded;
		foreach $i (sort keys %post_data) {	
                        printf LOGFILE "%7g     %15g     %15.2g\n",$i,$ncorrect_post{$i},100.*$ncorrect_post{$i}/$nincluded;
                }
                printf LOGFILE "\n\n";

                printf LOGFILE "STATISTICS:\n\n";
                printf LOGFILE "AVERAGE CORRECT QUESTIONS: %4.2g, MINIMUM CORRECT QUESTIONS: %4.2g, MAXIMUM CORRECT QUESTIONS: %4.2g\n\n",$ncorrect_mean_post,$ncorrect_min_post,$ncorrect_max_post;
                printf LOGFILE "AVERAGE CORRECT PERCENT: %4.2g, MINIMUM CORRECT PERCENT: %4.2g, MAXIMUM CORRECT PERCENT: %4.2g\n\n",100.*$ncorrect_mean_post/$nincluded,100.*$ncorrect_min_post/$nincluded,100.*$ncorrect_max_post/$nincluded;

                printf LOGFILE "\n\n";
                printf LOGFILE "DETAILED ANALYSIS OF FREQUENCY OF STUDENT CHOICES PER PROBLEM:\n\n";
                printf LOGFILE "PROBLEM ";
                for ($i=1;$i<=$nchoices;$i++) {
                        printf LOGFILE "** %s ",$choices[$i];
                }
                printf LOGFILE "\n\n";
                for ($i=1;$i<=$nproblems;$i++) {
			if ( $i ~~ @included_problems) {
	                        printf LOGFILE "%7g",$i;
        	                for ($j=1;$j<=$nchoices;$j++) {
                	                printf LOGFILE "%5g",$choice_matrix_post[$i][$j];
                        	}
                        	printf LOGFILE "\n";
			}
                }

                printf LOGFILE "\n\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\nEND OF SECTION 5: REPORT OF POST-TEST RESULTS\n";
                printf LOGFILE "*********" x 13;
                printf LOGFILE "\n\n";
        }
}
else {
	printf "\nERROR: UNKNOWN VALUE FOR FLAG (PRE)\n\n";
}

#CALCULATING THE GAIN WHEN DATA ARE AVAILABLE FOR BOTH PRE AND POST TESTS
$nstudents_gain = 0;
$gain_total = 0;
$gain_norm_total = 0;
$score_pre_total=0.;
$score_post_total=0.;

if ($flag_pre == 1 && $flag_post == 1) {
	foreach $i (sort keys %post_data) {
		if (exists $pre_data{$i}) {
			$gain{$i} = ($ncorrect_post{$i} - $ncorrect_pre{$i})/$nincluded;	
			$gain_total= $gain_total + $gain{$i};
			$gain_norm{$i} = ($ncorrect_post{$i} - $ncorrect_pre{$i})/($nincluded - $ncorrect_pre{$i});
			$gain_norm_total= $gain_norm_total + $gain_norm{$i};

			$score_pre_total = $score_pre_total + $ncorrect_pre{$i}/$nincluded;
			$score_post_total = $score_post_total + $ncorrect_post{$i}/$nincluded;			

			$nstudents_gain = $nstudents_gain + 1;
		}
	}

	#CALCULATIONS FOR RAW GAIN
	$gain_mean = $gain_total/$nstudents_gain;
	@gain_sorted = sort {$a <=> $b} (values %gain); #numerical sorting (not done by default!)
	$gain_min= $gain_sorted[0];
	$gain_max= $gain_sorted[-1];

	#USED TO CALCULATE HAKE's AVERAGE NORMALIZED GAIN
	$score_pre_mean = $score_pre_total/$nstudents_gain;
	$score_post_mean = $score_post_total/$nstudents_gain;
	$gain_mean_hake = $score_post_mean - $score_pre_mean;

	#CALCULATIONS FOR NORMALIZED GAIN
	$gain_norm_mean = $gain_norm_total/$nstudents_gain;
	@gain_norm_sorted = sort {$a <=> $b} (values %gain_norm); #numerical sorting (not done by default!)
	$gain_norm_min= $gain_norm_sorted[0];
	$gain_norm_max= $gain_norm_sorted[-1];

	$gain_norm_mean_hake = $gain_mean_hake/(1.-$score_pre_mean); #calculate Hake's average normalized gain

	printf LOGFILE "\n\n";
	printf LOGFILE "*********" x 13;
	printf LOGFILE "\nSECTION 6: REPORT OF GAIN RESULTS\n";
	printf LOGFILE "*********" x 13;
	printf LOGFILE "\n\n";
	printf LOGFILE "\nSTUDENT   **   GAIN (PERCENT)   ** NORMALIZED GAIN (PERCENT) **\n";
	foreach $i (sort keys %gain_norm) {
		printf LOGFILE "%7g   %15.2g      %15.2g\n",$i,100.*$gain{$i},100.*$gain_norm{$i};
	}
	printf LOGFILE "\n\nSTATISTICS:\n\n";
	printf LOGFILE "AVERAGE RAW GAIN PERCENT: %4.2g, MINIMUM RAW GAIN PERCENT: %4.2g, MAXIMUM RAW GAIN PERCENT: %4.2g\n",100.*$gain_mean,100.*$gain_min,100.*$gain_max;
	printf LOGFILE "AVERAGE NORMALIZED GAIN PERCENT: %4.2g, MINIMUM NORMALIZED GAIN PERCENT: %4.2g, MAXIMUM NORMALIZED GAIN PERCENT: %4.2g\n",100.*$gain_norm_mean,100.*$gain_norm_min,100.*$gain_norm_max;
	#printf LOGFILE "HAKE's METHOD: NORMALIZED GAIN PERCENT %4.2g, AVERAGE RAW GAIN PERCENT %5.3g",100.*$gain_norm_mean_hake,100.*$gain_mean_hake;
	printf LOGFILE "\n\n";
	printf LOGFILE "*********" x 13;
	printf LOGFILE "\nEND OF SECTION 6: REPORT OF GAIN RESULTS\n";
	printf LOGFILE "*********" x 13;

	printf LOGFILE "\n\nEND OF LOGFILE";
}
