# Recreational Decision Support Tool for Summer Flounder, Scup, and Black Sea Bass User Guide

## Welcome

This Decision Support Tool (DST) helps fishery management stakeholders answer a
practical question: if a state sets a particular combination of seasons, bag limits, 
and minimum sizes for recreational summer flounder, black sea bass, or scup next
fishing year, what is likely to happen? The DST produces an estimate of the fishing
mortality from the recreational fishery,  how many trips anglers would take, and 
how much better or worse off anglers would be under different combinations of 
fishing regulations. It is built for state and federal fishery managers working
through a regulation-setting cycle, and it does not assume any background in statistics or modeling.

This guide has three parts. The first explains how the tool works and why it gives the
answers it does. The second walks you through the screens. The third helps you
understand the results.

---

## How the Tool Works

### The question this tool helps you answer

Unlike Commercial regulations, recreational regulations work indirectly. Managers 
cannot set recreational harvest the way they set a quota. Instead managers set seasons, 
bag limits, and minimum sizes, and anglers respond by deciding whether to go
fishing at all, and once they are on the water, the rules determine which fish 
they can keep and which they must release. Recreational harvest is the result of all those
individual decisions together.

That indirect link is what makes recreational harvest hard to predict. A higher
minimum size reduces harvest.  However, it also increases the 
number of fish released, and some of those released fish die.  
A more restrictive set of rules may make trips less appealing overall, so 
fewer anglers go further reducing mortality.

There is a further complication specific to these three species: anglers often
catch them on the same trip. A regulation aimed at summer flounder changes what
a trip looks like for black sea bass too, because it changes the overall appeal
of going out. The tool handles all three species together for exactly this reason.

### What the tool is built on

Four kinds of information feed the tool.

**What anglers value.** In 2022, we surveyed anglers along the coast from Massachusetts to North Carolina. 
Each person was shown pairs of hypothetical fishing trips that differed in what the trip would cost and in 
how many summer flounder, black sea bass, and scup they would keep or have to release. We simply asked which trip 
they preferred, or whether they would rather do something other than saltwater fishing. These preferences are the behavioral engine 
of the tool. You can view a sample of the [survey](SFSBSBSurvey2022.pdf). 

Those answers reveal a great deal. Anglers value keeping a fish far more than catching and releasing one — for 
summer flounder, by roughly a factor of twelve. Of the three species, summer flounder is worth the most:
the first one kept on a trip is worth about \$35 to a typical angler, against about \$15 for the first black sea bass and 
well under a dollar for scup. Each additional fish is worth less than the one before it, so the fifth fish in the cooler 
does not add nearly as much as the first.

The survey also shows that summer flounder and black sea bass act as **substitutes**. Keeping more black sea bass 
makes each summer flounder worth slightly less, and the reverse. This is why the tool treats the three species as 
one connected fishery rather than three separate ones.

**What anglers actually catch.** Historical catch and effort estimates from the Marine Recreational
Information Program (MRIP) tell us how many trips are taken, when, and by which mode (for hire boat, private boat, or shore).  It 
also tells us how many Summer Flounder, Scup, and Black Sea Bass a trip typically catches. This is where  observed 
variability in trip outcomes comes from; for example many trips catch few fish, 
and a small number catch many.  We have launched the [RecDST data dashboard](https://connect.fisheries.noaa.gov/content/c257deee-a657-4c10-be8a-92827cb5bdfe/)
to help you understand the data that goes into this tool.

**How big the fish are.** Stock assessment projections supply the expected number
of Summer Flounder, Scup, and Black Sea Bass in the water next year for each Age Class. This matters a
great deal for recreational management, because how much a minimum size limit
affects anglers depends entirely on how many fish in the water are near that size.
Combined with historical information about which sizes anglers actually catch, 
this tells the tool what a typical catch looks like.  We know that recreational anglers 
are good at catching fish so we adjust historical catch to take account changes in biomass. For example, if the 
stock assessment contains a very large 3-year old class of fish in 2025, then in 2026, those 4-year old
fish will be a bit longer and the DST accounts for this.

**What trips cost.** Survey data on angler trip expenditures gives the tool a 
realistic spread of trip costs, which is what lets us convert changes in trip 
quality into dollars.

### What the simulation actually does

The simulation builds up a big answer from many small, realistic pieces. Rather 
than applying an average rule to the whole fishery, the tool imagines a very 
large number of individual fishing opportunities and works through each one, 
then adds up the results.

Here is a simplified version of what happens for a single opportunity — 
think of it as one person, on one day, deciding whether to go fishing.

1. **Set the scene.** The tool assigns this person a trip cost and some personal
characteristics like age and avidity level, drawn at random from realistic distributions. 
Age and avidity matter because they affect if people take or do not take a trip.

2. **Figure out what they would catch.** It draws a number of summer flounder, black sea bass, and scup 
from the catch-per-trip patterns for that state, time of year, and mode. Then it gives each individual fish 
a length, drawn from the size distribution of fish anglers are expected to encounter next year.

3. **Apply the rules.** Each fish is checked against the minimum size and bag limit in force on that date, for 
that mode and species. Legal fish are kept until the bag limit is reached; everything else is released.

4. **Decide whether the trip happens.** Now the tool knows what this trip would be like. 
Using the angler preferences from the survey, it works out how appealing the trip is compared with not 
fishing, and converts that into a probability that the trip takes place.
Because each trip contributes in proportion to how likely it is to happen, a set of regulations that makes 
fishing less attractive automatically produces fewer trips and less harvest. 

Once every opportunity has been worked through, the tool expands the results to
the size of the real fishery and converts numbers of fish into pounds. It also
applies discard mortality rates to estimate how many released fish do not survive.

**Calibration.** Before any of this is used to project fishing outcomes, the tool 
is verified against reality. It is set up so that the number of trips it produces
under baseline conditions matches the MRIP estimate for that year, and so that
the harvest it produces matches the MRIP harvest estimate for each state, mode, and species. 

**Repetition.** Finally, the whole process runs 100 times over. Each run uses a 
different plausible set of inputs, reflecting that MRIP estimates and stock 
projections are estimates rather than  exact counts. The result is a  spread of 
outcomes rather than a single number.

**Build a Baseline.** We always provide a set of results for "last year's" conditions 
and rules, giving a baseline for comparison for the same person on the same day.


### Assumptions worth knowing about

A few modeling choices shape how you should read the results.

**This is a one-year projection.** The tool estimates what happens next year given the projected stock. 
It does not carry that year's mortality forward into future stock size.

**Some things simply are not in the model.** Weather is the clearest example. A cold,
wet June will move effort in ways the tool cannot anticipate.

**Regulatory compliance is assumed.** The tool assumes anglers follow the rules.

**Everyone in a state, mode, and season is treated alike.** Within those categories, all anglers draw 
from the same catch and size distributions.  While differences in skill, location, and targeting 
are not modeled individually, the catch and size distributions have these attributes baked in. 

---

## Using the App

### Getting Started

The app opens on a Landing Page with Tabs for a Summary, Regulation selection, and Results.

![Landing Page with Tables for Results, Regulation Selection, and Summaries.](figures/ThreeTabLanding.png)

**Summary Page** is where you look at results. It shows every model run that has been completed, both 
coastwide and state by state.

**Regulation Selection** is where you build a new scenario and submit it.

**Results** is where you can combine any completed model runs for each state to see the summed estimates of all model results. 

Two things to know before you start.

**The summary page takes about a minute to load the first time** you open the app, because it reads in every
 completed run. This is normal.

**The app does not run the model while you wait.** When you submit a scenario, the app saves your 
regulations and puts your run in a queue. The model runs elsewhere and takes a substantial amount of 
time — each state is a separate job. Your results appear on the summary page once it finishes, which
 means you submit, go do something else, and come back later.


### Building a Scenario: the Regulation Selection Tab

The instructions at the top of this tab lay out the sequence: name your policy, 
pick your state(s), set your regulations, and click Run Me. Take them in that order 
— the regulation controls do not appear until you have selected a state.

#### Step 1: Name your run

The first box asks for a unique name. Use your initials and a number — `AB1`, `AB2`,
and so on. This name labels your run everywhere in the results, so pick something 
you will recognize. Each run needs a different name.

#### Step 2: Choose your states

Below the name box is a row of checkboxes: **MA, RI, CT, NY, NJ, DE, MD, VA, NC**.
Select every state you want in this run.

When you select a state, a colored panel of regulation controls appears further down 
the page for that state. Select several and you get several panels, each in its own 
color. Unselect a state and its panel disappears.

Selecting more states means a longer wait for results, since each state is modeled separately.

![Regulation selection tab with Policy name is AB1 and Delaware, Maryland, and Virginia are selected. MD is only partially show in this screen capture and VA is not shown.](figures/RegulationSelectionTabAB1.png)


#### Step 3: Set the regulations

Each state's panel is laid out in three columns:**Summer Flounder**, **Black Sea Bass**,
and **Scup**. We  pre-load current regulations. If you only want to test one
change, change that one control and leave the rest alone.

The walkthrough below uses **Massachusetts** as the example. Every state works
the same way; they differ only in how many seasons each species offers and in
whether the mode selector described below is available. The table at the end of
this section lists those differences. 


**The three modes.** Regulations are set separately for **For Hire**, **Private**, 
and **Shore**. Each gets its own season dates, bag limit, and minimum size.

**Setting a season.** A season is a calendar dropdown with two handles one for the opening day 
and one for closing day. All dates between these two dates are open. Unlike the fishing-year species 
elsewhere in the region, these seasons run on the calendar year.

**Setting a bag limit.** Type a number into the Bag Limit box — the number of fish kept per angler per day.

**Setting a minimum size.** Drag the Min Length slider. It moves in **half-inch steps**,
so 17.5 inches is available as well as 17 and 18.

![MA summer flounder regulations sliders and buttons.  Anglers can retain up to 5 Summer Flounder, greater than or equal to 17.5 inches, starting on May 24, 2027 and ending on September 23, 2027.](figures/MASummerFlounderSliders.png)


**Summer flounder in Massachusetts.** Three blocks appear, one per mode, each with 
a season, bag limit, and minimum size. 

**Black sea bass in Massachusetts.** This column starts with a dropdown reading
**"Regulations combined or separated by mode?"** with two choices:

- **All Modes Combined** gives you one set of controls — one season, one bag limit
, one minimum size — applied to every mode. Use this when the state's rules are
the same across modes. It is fewer controls to set and fewer chances to make a mistake.
- **Separated By Mode** gives you three sets, one per mode, exactly like summer flounder above.

Switching between the two swaps the controls in place.

Not every state and species offers this dropdown. Where it does not appear,
regulations are already set separately by mode.

**Scup in Massachusetts.** Scup in Massachusetts is set by default to two seasons. 

#### Adding more seasons

Below each species column is an **Add Season** button. Clicking it reveals another
block of season, bag limit, and minimum size controls for every mode of that
species at once. Click again to hide it.

The extra seasons come pre-set to December 31 with a bag limit of zero, which means
they do nothing until you change them. This is deliberate — revealing a season 
does not commit you to using it.

Each state and species has a maximum number of seasons, listed in the table below.
Once you have used them, the Add Season button has nothing further to reveal.

![MA summer flounder regulations sliders and buttons where add season has been selected so season 2 options display. Screenshot truncated for brevity.](figures/MASummerFlounderExtraSeason.png)

#### How seasons and closures are handled

Two rules govern how your dates become a fishing calendar.

**Any day not covered by a season is closed.** You do not need to enter a closure.
If a date falls outside every season you have set for a species and mode, no fish 
of that species may be kept that day. Anything caught is released.

**If seasons overlap, the higher-numbered season wins.** Say Season 1 runs May through 
October with a 30-fish bag and Season 2 runs July through August with a 10-fish bag.
July and August will use the 10-fish bag. This is useful when you want a broad season
with a more restrictive window carved out of it — set the general rules as Season 1 
and the exception as Season 2.

**To close a season you have opened,** set its bag limit to zero. Seasons with a 
zero bag limit are dropped from the regulations table in the results, so they will
not clutter your comparison.

#### Step 4: Submit the run

When your regulations are set, click **Run Me**.

**Click it once.** Clicking repeatedly submits the same run more than once. 
After one click, a message confirms your regulations were saved.

To submit another scenario, change the run name first, then adjust your regulations
and click Run Me again.

#### How states differ

Every state's panel works the way Massachusetts does. These are the differences.

| State | Summer flounder seasons | Black sea bass seasons | Scup seasons | Offers the mode dropdown for |
|---|---|---|---|---|
| **MA** Massachusetts | 2 | 2 | 3 For Hire, 2 Private and Shore | Black sea bass |
| **RI** Rhode Island | 2 | 3 | 4 For Hire, 2 Private and Shore | Summer flounder |
| **CT** Connecticut | 3 | 3 | 4 For Hire, 2 Private and Shore | Summer flounder |
| **NY** New York | 3 | 3 | 4 For Hire, 2 Private and Shore | Summer flounder, black sea bass |
| **NJ** New Jersey | 2 | 5 | 3 | All three species |
| **DE** Delaware | 3 | 3 | 2 | All three species |
| **MD** Maryland | 3 | 3 | 2 | All three species |
| **VA** Virginia | 3 | 3 | 2 | All three species |
| **NC** North Carolina | 2 | 3 | 2 | All three species |

Where a species does not offer the mode dropdown, its regulations are always set 
separately for For Hire, Private, and Shore. 

### Viewing Results: the Summary Page

The Summary Page has two levels. The top of the page compares every state at once.
Below that is a second row of tabs — one per state, plus a Regulations tab — where 
you can look at a single state in detail.

**Everything on this page is measured against the status quo run.** The tool compares
each of your scenarios against a baseline run named "SQ." 

![Summary page showing Median change in harvest for all states and a summary table showing median percent change.](figures/SummaryGraphsTable.png)

The Results tab has a panel on the left with all states. To explore the results, select a policy for each state or group of states you
would like to see results for. Once selected, click the Calculate button. This button provides the outputs summed across states with policies selected.

The dropdown menu contains all of the completed model runs. If you do not see the one you are looking for it may be because
that model hasn't completed. Come back to it at a later time and the Policy should be available. 

The results show median harvest weight in pounds and the percent change from the status quo, angler satisfaction, predicted trips,
median discard weight and median dead discard weight in pounds as well as a table of the regulations for the selected policies.

![The Results tab combines policy "JB5" in MA, "JB5" in RI, and "SQ" in CT. All metrics presented are just for these three states.  Screenshot has been truncated for brevity.](figures/ResultsTab.png)

---

## Interpreting Your Results

Start at the top of the Summary Page. The coastwide figure and the summary table
tell you, in one look, whether any of your scenarios move harvest in the direction
and by the amount you need. Once you have narrowed the field, go into the state 
tabs for the detail.

Every figure in this tool shares the same horizontal axis: **percent change in harvest from the status quo**. 
Zero means the scenario produces the same harvest as current regulations. Negative 
values are reductions; positive values are liberalizations. This is deliberate —
it lets you see, for any given harvest change, what else comes with it.

Once you have a few policy scenarios that meet the required percent change, explore those policies deeper using the 
Results tab. See how those individual policies add up with other states. 

### Understanding Uncertainty in Your Results

The tool does not run once. It runs the whole simulation **100 times**, and each
run is called a draw.

**Why there is more than one run.** Several of the tool's inputs are estimates
rather than exact counts. MRIP catch and effort figures come from a survey of 
a sample of anglers, so they carry sampling variability — the estimate of how 
many trips a state took in a given month has a range around it, sometimes a wide 
one. Stock assessment projections of how many fish will be in the water next year,
and at what sizes, come with their own range. So does the average weight of a harvested fish.

For each of the 100 draws, the tool pulls a different plausible value for each of
these inputs, then simulates the entire fishery from start to finish using those
values. The result is 100 answers rather than one — a range of outcomes, all consistent 
with what is actually known about the fishery.

**What the reported numbers are.** Every number in the tables and figures is a
**median** across the 100 draws. The median is the middle value: half the draws 
came in higher, half came in lower. A median is used rather than an average because 
a few extreme draws pull an average around, while the median stays put.

**How the percent changes are calculated.** The tool does not compare one summary
number against another. It compares your scenario against the status quo run **draw by draw**. 
This means draw 1 of your scenario is compared against draw 1 of the status quo, draw 2 against draw 2, and so on. 
This produces 100 percent changes. What you see is the median of those. This matters 
because it holds the uncertain inputs constant on both sides of each comparison. 
The percent change you are reading is the effect of the regulations, with the 
year-to-year uncertainty removed from the comparison rather than left to muddy it.

**What this means for reading results.** A median percent change of −12 percent
means that in half the simulations the reduction was larger than 12 percent and 
in half it was smaller. It does not mean the reduction will be exactly 12 percent.
When two scenarios differ by a percentage point or two, that difference is small 
relative to what the tool can resolve, and the two should be treated as roughly 
equivalent. Differences of several percentage points or more are meaningful.

If you need the full spread rather than the middle, download the result file. Every one of the 100 simulations is in there.

---

## Questions or Feedback

If you have questions about this tool, need help interpreting a run, or want to 
suggest an improvement, contact the RecDST team at nefsc.recdst@noaa.gov.
