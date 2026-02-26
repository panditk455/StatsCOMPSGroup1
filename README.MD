# Exploring Error Rates in Forensic Science  
**STATS COMPS**  
Kritika Pandit, Cici Wang, Nicholas  
Prof. Amanda Luby  
17 Feb 2026  

---

## Project Context

The Hawthorne Effect is a phenomenon where individuals alter their behavior—typically increasing performance or productivity—due to their awareness of being observed or receiving special attention from researchers or supervisors. Forensic science has been working on measuring the error rates of human factors in identifying firearms, fingerprints, and others. However, introducing the Hawthorne effect would change the previous study’s assumptions greatly. Most of the previous studies are conducted under the “discovered” assumption, that is all examiners are aware that they are being tested. The only study (Scurich et. al) which evaluates this effect suggests that the inconclusive rate for discovered items was 56.4%, while the inconclusive rate for undiscovered test items was 39.3%. Motivated by this study, we hope to further investigate how specifically would Hawthorne Effect influence error rate (false positive and false negative rates) and the potential interacting factors of test banks’ quality, difficulty, and test designs.  

---

## Literature Review

https://onlinelibrary.wiley.com/doi/10.1111/1556-4029.70047  
Directly quantifies how knowing one is in a test changes inconclusive rates in a real blind-embedded proficiency program (56.4% inconclusive when tests are discovered vs 39.3% when undiscovered), and explicitly frames this as a Hawthorne/“strategic examiner” effect on decision thresholds.

https://www.sciencedirect.com/science/article/pii/S2589871X22000584?via%3Dihub  
Argues that “inconclusive” decisions in firearms studies can hide or distort true error rates, and that because examiners know they are being tested, they likely shift behavior toward safer “inconclusive” calls very similar to that described by the Hawthorne effect.

https://www.sciencedirect.com/science/article/pii/S0895435613003545?via%3Dihub  
Argues that there is no single, unitary Hawthorne effect, but it does show that research participation itself can change people’s behavior in small, context-dependent ways, which is exactly the core idea behind the Hawthorne effect. Doesn’t specifically target forensic black-box studies.

https://scholarlycommons.law.northwestern.edu/cgi/viewcontent.cgi?article=1023&context=facultyworkingpapers  
Argues that well-designed, blind proficiency tests are essential for estimating real error rates in forensic disciplines, with an emphasis on behavior changes when people know they are being tested. Doesn’t specifically mention the Hawthorne effect, but directly describes it.

---

## Simulation 1 – Using Bernoulli Distribution to Quantify Hawthorne Effect

We used the 2024 bullets dataset as our real-data baseline and then layered resampling and a simple Hawthorne-effect simulation to study how test awareness affects estimated error rates and inconclusive rates, and how reliable different confidence interval (CI) procedures are when responses are clustered by examiner and by item set.

We cleaned and combined the KQ and QQ response sheets and recoded each response into three outcome types (ID, Exclusion, Inconclusive). Using the ground truth from `Mating` (mated vs non-mated), we defined false positives as ID calls on non-mated comparisons and false negatives as Exclusion calls on mated comparisons.

From the baseline data (n = 2884), we estimated:
- FP rate = 0.0668 (128 FP)
- FN rate = 0.0496 (48 FN)
- Overall inconclusive rate = 0.409 (1179 inconclusive)

We also saw inconclusives differed by truth:
- 0.266 for same-source (mated)
- 0.481 for different-source (non-mated)

### CI Methods Implemented

1. Naive nonparametric bootstrap (resample rows)
2. Clustered bootstrap by examiner (AnonID)
3. Clustered bootstrap by item sets (Cset)

For FP rate:
- Naive bootstrap 95% CI: 0.0557–0.0765  
- Item-clustered CI: 0.0559–0.0786  
- Examiner-clustered CI: 0.0383–0.1039  

This suggests examiner-level clustering substantially increases uncertainty and that naive resampling can be overly optimistic when examiner effects induce correlation.

### Hawthorne Simulation

We simulated a “discovered/aware” condition by randomly labeling some part of the trials as discovered and then flipping a fraction of conclusive calls to inconclusive with probability delta = 0.25.

Under this condition:
- Inconclusive rate increased from 0.409 to 0.434 (1179 to 1252)
- FP decreased from 0.0668 to 0.0652 (128 to 125)
- FN decreased from 0.0496 to 0.0455 (48 to 44)

Inconclusive rates increased within each truth category:
- 0.266 → 0.300 (same-source)
- 0.481 → 0.502 (different-source)

---

## Simulation 2

To estimate uncertainty in error rates without imposing parametric assumptions, we employed a nonparametric bootstrap framework tailored to the clustered structure of the study data. The dataset consists of repeated decisions nested within examiners (AnonID) and comparison sets (Cset), so standard i.i.d. resampling would underestimate variability.

We generated B = 2000 bootstrap resamples and saved indices for reproducibility.

### Bootstrap Methods

- Row-level bootstrap  
- Clustered bootstrap by examiner (AnonID)  
- Clustered bootstrap by comparison set (Cset)  

For each replicate:
- Reconstruct dataset  
- Compute inconclusive rate  
- Compute definitive error rate  
- Compute conditional error rates  

Construct percentile-based 95% confidence intervals.

All bootstrap resamples were generated and stored prior to any Hawthorne simulation.

### Undiscovered Condition Simulation

For each bootstrap replicate:
- Compute observed inconclusive rate  
- Reverse Hawthorne inflation  
- Reassign a fraction of inconclusives to definitive decisions (ID, LeanID, Excl, LeanExcl)  
- Apply Bernoulli draw with probability g  

### Preliminary Results

- Discovered condition produces ~11–12 percentage points higher inconclusive rates  
- Hawthorne reduces false identifications by ~10 percentage points  
- Observation reduces false exclusions (~2–3 percentage points)  
- Effect robust to examiner-level and item-level dependence  

---

## Error Rate Case Bootstrap Results

- By row  
- By examiner  
- By Cset  

---

## Evaluation Methods

Finding a new dataset and comparing it with our existing findings, justifying whether or not our results are applicable.

---

## New Statistical Topic

We will explore mixed-effects modeling to explicitly model dependence between observations. Unlike bootstrap methods that approximate uncertainty through resampling, hierarchical models estimate variability attributable to examiners and evidence items directly. This allows us to quantify how much of the variation in forensic decisions arises from examiner behavior versus evidence characteristics, and how the Hawthorne effect shifts decision thresholds.

Link:  
https://journal.r-project.org/articles/RJ-2023-015/

---

## Timeline of the Project

The schedule is intentionally made in a way in order to ensure that we complete the major components early. We want to allow sufficient time for revision and feedback before deadlines arrive.

### Week 7
- Finalize topic and scope  
- Complete proposal  
- Implement proof-of-concept simulation  
- Ensure reproducibility  

### Week 8
- Meet with Amanda  
- Decide final modeling strategy  
- Plan next steps  
- Begin simulation framework  

### Week 9
- Continue simulation development  
- Validate toy simulation  
- Create exploratory plots  

### Week 10
- Literature review  
- Finalize simulation structure  
- Produce initial figures  

### Break
- Review progress  
- Clean code  
- Organize results  
- Draft methods  

### Week 1
- Write Results section  
- Interpret FP/FN/inconclusive shifts  

### Week 2
- Continue writing  
- Meet with Amanda  
- Revise  

### Week 3
- Complete rough draft  
- Improve readability  

### Week 4
- Edit based on feedback  
- Strengthen argument  

### Week 5
- Final revisions  
- Begin poster design  

### Week 6
- Refine poster  
- Obtain feedback  

### Week 7
- Poster presentation  
- Incorporate feedback  

### Week 8
- Peer review  
- Finalize figures  

### Week 9
- Final editing and formatting  

### Week 10
- Submit final paper, DONE!

---

## Resources

Dataset to be using – 2024 Bullet Dataset.  
We might use other datasets or papers if we need more extension to our project.