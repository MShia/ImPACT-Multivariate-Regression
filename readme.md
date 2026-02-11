# Web-based Tool for ImPACT Interpretation
## Comparative Normative Framework Using Multivariate and Univariate Regression

---

## Overview

This interactive Shiny application provides a comprehensive normative assessment framework for cognitive performance in jockeys, implementing two complementary statistical approaches: **multivariate regression with tolerance regions** and **univariate regression with percentile-based classification (Iverson method)**. The tool enables clinicians and researchers to assess whether an individual jockey's cognitive performance falls within expected normative ranges or shows atypical patterns that may warrant further evaluation.

---

## Scientific Background

### The Need for Dual Methodology

Cognitive assessment in athletes, particularly jockeys, benefits from examining performance through multiple statistical lenses. While traditional approaches focus on isolated deficits in individual cognitive domains, recent methodological advances recognize that cognitive functions operate as integrated systems. This tool bridges both perspectives by implementing:

1. **Multivariate Approach** - Considers the combined cognitive profile across all domains simultaneously
2. **Univariate Approach** - Examines performance in each cognitive domain independently

These complementary methods provide a more complete clinical picture, as they can identify different patterns of cognitive dysfunction that may have distinct clinical implications.

---

## Methodological Framework

### Method 1: Multivariate Regression with Tolerance Regions

**Statistical Foundation:**
- **Regression Model**: Simultaneously models all four cognitive outcomes (visual memory, visual motor speed, verbal memory, reaction speed) as a function of demographic predictors (age, sex, education)
- **Tolerance Regions**: Non-parametric multivariate tolerance regions applied to regression residuals using depth-based methods (specifically, semi-space depth with Mahalanobis distance)
- **Personalized Cutoffs**: Each individual receives personalized lower cutoff values for each cognitive domain based on their demographic profile

**What It Detects:**
- **Combined cognitive profile deviations** - Identifies when the overall pattern of cognitive performance is atypical relative to demographically similar peers
- **Multivariate outliers** - Detects unusual combinations of scores across domains, even when individual scores might appear normal
- **Profile-based abnormalities** - Captures scenarios where multiple domains show subtle deviations that together indicate atypical performance

**Clinical Interpretation:**
- **INSIDE Region**: All cognitive scores meet or exceed personalized cutoffs → Performance consistent with normative expectations
- **OUTSIDE Region**: At least one cognitive score falls below personalized cutoff → Atypical performance pattern requiring clinical attention

**Key Advantage:** Accounts for correlations between cognitive domains, providing a holistic assessment of cognitive function that reflects how these abilities interact in real-world performance.

---

### Method 2: Univariate Regression with Iverson Classification

**Statistical Foundation:**
- **Regression Models**: Separate regression model for each cognitive outcome as a function of demographic predictors
- **Standardization**: Residuals are standardized and converted to percentile rankings
- **Classification Rules**: Iverson criteria applied to identify severity of cognitive impairment based on the number and magnitude of low percentile scores

**Iverson Classification System:**

| Classification | Criteria |
|----------------|----------|
| **Broadly Normal** | ≤2 scores ≤25th percentile AND ≤1 score ≤16th AND 0 scores ≤10th |
| **Below Average** | ≥3 scores ≤25th OR 2 scores ≤16th OR 1 score ≤10th |
| **Well Below Average** | ≥3 scores ≤16th OR 2 scores ≤10th OR 1 score ≤5th |
| **Unusually Low** | ≥3 scores ≤10th OR 2 scores ≤5th OR 1 score ≤2nd |
| **Extremely Low** | ≥3 scores ≤5th OR ≥2 scores ≤2nd |

**What It Detects:**
- **Isolated cognitive deficits** - Identifies specific domains where performance is impaired
- **Severity of impairment** - Quantifies how far below normal each domain performs
- **Number of affected domains** - Counts how many cognitive areas show deficits

**Clinical Interpretation:**
The classification provides a standardized severity rating that indicates the extent and distribution of cognitive deficits across domains. Lower percentile scores in multiple domains suggest more severe impairment.

**Key Advantage:** Straightforward interpretation with established clinical thresholds, allowing direct comparison to normative percentiles and identification of specific weaknesses.

---

## Complementary Decision-Making

### Why Both Methods Matter

The two approaches provide complementary information that enhances clinical decision-making:

| Aspect | Multivariate Approach | Univariate Approach |
|--------|----------------------|---------------------|
| **Focus** | Overall cognitive profile | Individual domain performance |
| **Sensitivity** | Detects subtle multi-domain patterns | Detects pronounced single-domain deficits |
| **Clinical Use** | Screens for atypical cognitive functioning | Identifies specific areas of weakness |
| **Interpretation** | Dichotomous (inside/outside) | Graded severity scale |

### Example Clinical Scenarios

**Scenario 1: Concordant Results**
- Multivariate: OUTSIDE region
- Univariate: Unusually Low
- **Interpretation**: Clear evidence of cognitive impairment requiring clinical attention. Multiple cognitive domains show deficits that both stand out individually and create an atypical overall profile.

**Scenario 2: Multivariate Detection**
- Multivariate: OUTSIDE region
- Univariate: Below Average
- **Interpretation**: The combination of scores is atypical even though individual domains may not show severe impairment. Suggests a profile inconsistency that warrants further evaluation. May indicate subtle multi-domain dysfunction not captured by univariate thresholds.

**Scenario 3: Univariate Detection**
- Multivariate: INSIDE region
- Univariate: Well Below Average
- **Interpretation**: One or two domains show pronounced deficits, but the overall cognitive profile remains within normative bounds. Suggests isolated domain-specific weaknesses that may benefit from targeted intervention.

**Scenario 4: Both Normal**
- Multivariate: INSIDE region
- Univariate: Broadly Normal
- **Interpretation**: Cognitive performance is within expected normative ranges across all assessment methods. No evidence of clinically significant impairment.

---

## Cognitive Domains Assessed

### Visual Memory
The ability to remember and recall visual information. Important for recognizing track features, other jockeys, and environmental cues during racing.

### Visual Motor Speed
The speed of processing visual information and executing motor responses. Critical for rapid decision-making and reaction during high-speed racing situations.

### Verbal Memory
The ability to remember and recall verbal information. Relevant for remembering instructions, race strategies, and post-race debriefing.

### Reaction Speed
Response time to stimuli (measured in seconds; lower is better). Fundamental for quick responses to changing race conditions and avoiding accidents.

**Note**: Reaction speed is reverse-scored in the analysis (multiplied by -1) so that, like other domains, higher values represent better performance.

---

## Application Features

### Data Input
- **Upload Custom Data**: Users can upload their own cognitive assessment data in CSV format
- **Required Variables**: age, sex, education, visual_memory, visual_motor_speed, verbal_memory, reaction_speed
- **Data Validation**: Automatic checks for data quality and completeness
- **Demographic Constraints**: Ensures logical age-education relationships (age - education ≥ 5 years)

### Statistical Parameters
- **Adjustable Confidence Level (1-α)**: Set the confidence level for tolerance region construction (default: 95%)
- **Adjustable Coverage Probability (P)**: Set the proportion of the population expected to fall within tolerance bounds (default: 95%)
- **Real-time Recalculation**: Tolerance regions update dynamically when parameters are changed

### Individual Assessment
- **New Subject Evaluation**: Enter demographics and cognitive scores to assess a new individual
- **Dual Classification**: Simultaneously view results from both multivariate and univariate methods
- **Visual Indicators**: Color-coded panels (green = normal, red = atypical) for quick interpretation
- **Score Highlighting**: Individual cognitive domain scores that drive classification are highlighted in red
- **Educational Tooltips**: Hover over info icons to view classification criteria and interpretation guidelines

### Dataset Analysis
- **Complete Dataset View**: Interactive table showing all subjects with classification results
- **Export Capability**: Download full analysis results as CSV for further analysis or reporting
- **Sortable/Filterable**: Easy navigation through large datasets

---

## Clinical Significance

### Establishing Jockey-Specific Norms

Traditional cognitive norms are derived from general populations that may not reflect the unique characteristics of jockeys:
- **Physical demands**: Extreme weight control requirements
- **Occupational hazards**: High concussion risk from falls
- **Selection effects**: Cognitive abilities required for success in racing

This tool uses **jockey-specific normative data** from Irish horseracing (2021-2024), providing appropriate reference standards for this unique athletic population.

### Concussion Management

Jockeys have one of the highest concussion rates among athletes. This tool supports:
- **Baseline Assessment**: Establishing pre-injury cognitive profiles
- **Post-Injury Evaluation**: Comparing post-concussion performance to individual baseline and population norms
- **Return-to-Ride Decisions**: Providing objective data to inform clearance decisions
- **Longitudinal Monitoring**: Tracking cognitive recovery over time

### Research Applications

- **Cross-Method Validation**: Compare multivariate and univariate approaches in the same sample
- **Methodological Studies**: Investigate which method better predicts clinical outcomes
- **Population Characterization**: Describe the cognitive profile of jockey populations
- **Risk Factor Analysis**: Examine how demographics relate to cognitive performance

---

## Interpretation Guidelines

### Using Both Methods Together

**Step 1: Run Both Analyses**
- Input or upload cognitive assessment data
- Review multivariate tolerance region classification
- Review Iverson percentile classification

**Step 2: Compare Results**
- Are results concordant (both normal or both abnormal)?
- If discordant, which method detected the issue?
- Which cognitive domains are flagged?

**Step 3: Clinical Integration**
- Consider the pattern of deficits
- Review highlighted scores for severity
- Integrate with other clinical information (symptoms, mechanism of injury, imaging)
- Make evidence-informed decisions about management

**Step 4: Communication**
- Explain findings to athlete using both frameworks
- Highlight specific areas of concern
- Discuss clinical implications and recommendations
- Document findings for longitudinal comparison

---

## Statistical Advantages

### Multivariate Tolerance Regions

**Strengths:**
- Accounts for between-domain correlations
- Adjusts for demographic characteristics
- Detects subtle multi-domain deviations
- Non-parametric approach (fewer distributional assumptions)
- Provides individual-specific cutoffs

**Limitations:**
- Dichotomous classification (in/out)
- Less intuitive for some clinicians
- Requires sufficient sample size for stable estimation

### Univariate Percentile Classification

**Strengths:**
- Widely understood percentile metrics
- Graded severity classification
- Identifies specific domain weaknesses
- Established clinical thresholds (Iverson criteria)
- Transparent interpretation

**Limitations:**
- Ignores correlations between domains
- May miss subtle multi-domain patterns
- Assumes independence across domains
- Requires multiple comparisons across domains

**The Complementary Solution:**
Using both methods leverages their respective strengths while compensating for individual limitations, providing a more robust and comprehensive assessment framework.

---

## Data Requirements

### Required CSV Columns

| Column Name | Type | Description | Example Values |
|-------------|------|-------------|----------------|
| `age` | Numeric | Age in years | 25, 30, 35 |
| `sex` | Character | Sex (lowercase) | 'm', 'f' |
| `education` | Numeric | Years of education | 12, 16, 18 |
| `visual_memory` | Numeric | Visual memory composite score | 85.5, 78.2 |
| `visual_motor_speed` | Numeric | Visual motor speed score | 45.3, 42.1 |
| `verbal_memory` | Numeric | Verbal memory composite score | 90.1, 85.3 |
| `reaction_speed` | Numeric | Reaction time (seconds) | 0.55, 0.58 |

### Data Quality Requirements

- **No missing values**: All rows must have complete data
- **Logical age-education**: Age - Education ≥ 5 (assuming school starts at age 5)
- **Minimum sample size**: At least 10 valid subjects required for analysis
- **Realistic ranges**: Values should be plausible for the assessment instruments used

---

## Technical Implementation

### Statistical Methods

**Multivariate Tolerance Regions:**
- Semi-space depth function with Mahalanobis distance
- Non-parametric tolerance region construction
- Implemented via the `tolerance` R package
- Confidence level and coverage probability user-adjustable

**Univariate Percentile Calculation:**
- Standardized residuals from separate linear regression models
- Normal distribution assumption for percentile conversion
- Iverson classification rules applied to percentile matrix

**Regression Models:**
- Linear regression with demographic predictors: age + sex + education
- Separate models for multivariate (using `lm()` with `cbind()` outcome) and univariate approaches
- Residual-based analysis for both methods

### Software

- **R Shiny**: Interactive web application framework
- **tolerance**: Multivariate tolerance region construction
- **DT**: Interactive data tables for dataset visualization
- **Base R**: Statistical computation and regression modeling

---

## Research Context

### ImPACT Assessment

This tool is designed for use with ImPACT (Immediate Post-Concussion Assessment and Cognitive Testing), a computerized neurocognitive test battery widely used in sports medicine. The four cognitive composite scores assessed by this tool correspond to ImPACT's primary outcome measures.

### Normative Data Source

The normative framework is based on cognitive assessment data collected from Irish jockeys between 2021 and 2024 through the Irish Horseracing Regulatory Board's baseline testing program. This represents the first jockey-specific normative dataset for ImPACT assessment, providing population-appropriate reference standards.

### Institutional Context

Developed at Dublin City University's School of Health and Human Performance, in collaboration with the Insight SFI Research Centre for Data Analytics. This work is part of ongoing research into concussion assessment, athlete monitoring, and sports medicine in high-risk athletic populations.

---

## Usage Workflow

### For Clinical Assessment

1. **Upload normative data** (or use pre-loaded jockey norms)
2. **Adjust tolerance parameters** if desired (default: 95% confidence, 95% coverage)
3. **Enter athlete demographics** (age, sex, education)
4. **Input cognitive test scores** (from ImPACT or similar assessment)
5. **Click "Assess"** to generate dual classification
6. **Review both panels**:
   - Multivariate panel: INSIDE or OUTSIDE tolerance region
   - Iverson panel: Classification category with highlighted low scores
7. **Integrate findings** with clinical judgment
8. **Document results** for longitudinal comparison
9. **Download dataset** with all classifications for records

### For Research

1. **Upload study dataset** with cognitive assessment data
2. **Review full dataset table** showing classifications for all subjects
3. **Compare concordance** between multivariate and univariate methods
4. **Analyze highlighted domains** across classification groups
5. **Export results** for statistical analysis
6. **Generate visualizations** and reports for manuscripts
7. **Share normative framework** with other researchers via cloud deployment

---

## Interpretation Example

### Case Study: Post-Concussion Evaluation

**Athlete Profile:**
- Age: 28 years
- Sex: Male
- Education: 14 years
- Days post-concussion: 7

**Cognitive Test Scores:**
- Visual Memory: 72.0 (Percentile: 6.2%)
- Visual Motor Speed: 40.5 (Percentile: 8.8%)
- Verbal Memory: 85.0 (Percentile: 45.3%)
- Reaction Speed: 0.62 sec (Percentile: 11.2%)

**Multivariate Assessment:**
- Classification: **OUTSIDE Region**
- Interpretation: The combined profile of cognitive scores is atypical compared to demographically similar jockeys. Multiple domains show below-expected performance that together create an unusual pattern.

**Univariate Assessment:**
- Classification: **Unusually Low**
- Highlighted Domains: Visual Memory (6.2%), Visual Motor Speed (8.8%), Reaction Speed (11.2%)
- Interpretation: Three cognitive domains show performance ≤10th percentile, meeting criteria for unusually low classification. Visual memory and visual motor speed are particularly impaired.

**Clinical Integration:**
- **Concordant findings**: Both methods indicate cognitive impairment
- **Specific deficits**: Visual-spatial processing and processing speed most affected
- **Clinical decision**: Not cleared for return to riding. Recommend continued rest and reassessment in 3-5 days
- **Follow-up**: Track recovery of visual memory and processing speed specifically

---

## Advantages Over Single-Method Approaches

### Traditional Approach: Univariate Only
**Limitations:**
- May miss multivariate patterns where no single domain is severely impaired
- Ignores correlations between cognitive functions
- Cannot detect unusual profiles that fall within individual domain norms

### This Dual-Method Approach
**Benefits:**
- Captures both isolated deficits (univariate) and profile abnormalities (multivariate)
- Provides graded severity (Iverson) and dichotomous screening (tolerance region)
- Enhances clinical sensitivity by detecting different patterns of dysfunction
- Offers convergent evidence when both methods agree
- Provides differential insights when methods disagree

---

## Ethical Considerations

### Appropriate Use

This tool is designed to **support**, not replace, clinical judgment. Cognitive assessment is one component of comprehensive concussion management. Results should be interpreted within the context of:
- Symptom presentation
- Clinical examination findings
- Medical history
- Mechanism of injury
- Sport-specific demands
- Individual athlete factors

### Limitations

- Results are only as good as the normative data quality
- Population-specific norms are required (e.g., jockey norms for jockeys)
- Cannot diagnose concussion or other conditions
- Should not be the sole basis for return-to-sport decisions
- Requires proper training in cognitive assessment interpretation

### Data Privacy

- No personal identifying information should be included in uploaded datasets
- Data is processed in-session only (not stored on servers)
- Users are responsible for ensuring ethical approval for data collection
- Comply with GDPR and local data protection regulations

---

## Future Directions

### Methodological Enhancements
- Incorporation of longitudinal data analysis
- Addition of reliable change indices for individual athletes
- Machine learning integration for predictive modeling
- Exploration of alternative depth functions and tolerance region methods

### Clinical Extensions
- Integration with symptom scales and vestibular testing
- Development of return-to-sport algorithms incorporating cognitive data
- Validation against long-term outcomes
- Expansion to other athlete populations

### Research Applications
- Multi-site validation studies
- Cross-cultural normative development
- Investigation of risk factors for cognitive decline
- Examination of cognitive reserve in athletic populations

---

## Citation

If using this tool in research or clinical practice, please cite:

> [Shah, M.H, Davood, R.S, et.al.]. (2025). Improving Concussion Assessment in Jockeys: Normative Benchmarks and Web-Based Tool for ImPACT Interpretation Dublin City University, School of Health and Human Performance. Insight SFI Research Centre for Data Analytics.

---

## Contact

**Research Team:**
School of Health and Human Performance  
Dublin City University  
Dublin, Ireland


**For Questions:**
- Methodological inquiries: [maqsood.hussainshah@dcu.ie]


---

## Acknowledgments

This work was made possible through collaboration with:
- Irish Horseracing Regulatory Board
- Dublin City University
- Insight SFI Research Centre for Data Analytics
- ImPACT Applications, Inc.

---

**Version:** 1.0  
**Last Updated:** December 2024  
**License:** MIT License

---

## Summary

This tool provides a **dual-method normative framework** for assessing cognitive performance in jockeys, implementing both multivariate regression with tolerance regions and univariate regression with Iverson percentile classification. By offering complementary perspectives on cognitive function—one focused on combined cognitive profiles and the other on isolated domain deficits—the tool enables more comprehensive and clinically meaningful assessment. This is particularly valuable in concussion management and athlete monitoring, where both subtle multi-domain patterns and pronounced single-domain impairments may indicate clinically significant dysfunction requiring intervention.
