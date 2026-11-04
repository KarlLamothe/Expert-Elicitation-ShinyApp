# Google Sheets Setup Guide

## Overview

Version 2 of the Expert Elicitation Shiny App can use Google Sheets as an optional live data source.

This workflow can be useful during an active expert-elicitation exercise because participants can enter or revise responses while the facilitator periodically retrieves the most recent responses into the Shiny application.

Google Sheets is **not required** to use the application. Facilitators who do not wish to configure Google authentication can instead use the CSV upload workflow described in the Version 2 README.

A recommended Google Sheets workflow uses:

1. a separate Google spreadsheet for each participant;
2. a separate facilitator spreadsheet that consolidates participant responses; and
3. the Shiny application, which reads the consolidated facilitator data.

A typical structure is:

```text
Participant P1 spreadsheet ──┐
Participant P2 spreadsheet ──┤
Participant P3 spreadsheet ──┤
Participant P4 spreadsheet ──┼──► Facilitator spreadsheet ──► Shiny application
Participant P5 spreadsheet ──┤
Participant P6 spreadsheet ──┘
```

This structure allows participants to work independently without routinely viewing the individual responses submitted by other participants.

---

# 1. Configure Google Authentication

Google Sheets access is handled through the `googlesheets4` R package.

Before using Google Sheets, open:

```text
App Version 2/Run-Application.R
```

Locate:

```r
gs4_auth(email = "YOUR_GOOGLE_EMAIL")
```

Replace `YOUR_GOOGLE_EMAIL` with the email address of the Google account that has access to the facilitator spreadsheet.

For example:

```r
gs4_auth(email = "example@gmail.com")
```

The Google account used here must have permission to access the facilitator spreadsheet.

Do not commit a personal email address, authentication token, password, or other credential to the public GitHub repository.

---

# 2. First-Time Authentication

The first time `googlesheets4` requires authentication, a web browser may open and ask the user to:

1. sign in to a Google account;
2. select the account specified in `gs4_auth()`, if necessary;
3. authorize access to Google Sheets; and
4. return to the R session after authorization is complete.

After successful authentication, the authorization token can be cached locally.

This generally means that the full browser-based authentication procedure does not need to be repeated every time the application is launched.

---

# 3. Authentication Tokens

Google authentication is managed by `googlesheets4` through the `gargle` authentication system.

After authentication, OAuth credentials can be stored in a local credential cache rather than inside the Shiny application project.

The cached token:

- is associated with the Google identity used during authentication;
- allows authorization to be reused across R sessions;
- can be refreshed when required; and
- should be treated as a credential.

Authentication tokens should **never be copied into the application code or committed to GitHub**.

The repository `.gitignore` is configured to exclude common authentication and credential files. However, users should still review changes in Git before committing or pushing files to a remote repository.

If authentication needs to be repeated, `googlesheets4` can be configured to ignore or replace an existing cached authorization and begin a new authentication process.

---

# 4. Create the Participant Spreadsheets

Create a separate Google spreadsheet for each participant.

For example:

```text
Expert Elicitation - P1
Expert Elicitation - P2
Expert Elicitation - P3
Expert Elicitation - P4
Expert Elicitation - P5
Expert Elicitation - P6
```

Each participant should receive access only to the spreadsheet required for entering that participant's responses.

A participant response table can contain:

```text
Question
Round
Participant
Lowest_Plausible_Pr
Best_Guess_Pr
Highest_Plausible_Pr
Assessment_Confidence
Notes
```

For example:

```text
Question | Round | Participant | LPP  | BGP  | HPP  | AC  | Notes
1        | 1     | P1          | 0.20 | 0.40 | 0.60 | 70  |
2        | 1     | P1          | 0.30 | 0.55 | 0.75 | 80  |  
3        | 1     | P1          | 0.15 | 0.30 | 0.50 | 60  |
```

The exact column names do not have to match these names because the Shiny application provides column-mapping controls. However, using consistent names across participant files makes the facilitator workflow substantially easier.

`Round` is optional when only one elicitation round is being conducted.

`Assessment_Confidence` is also optional when assessment confidence weighting is not being used.

`Notes` is also optional.

---

# 5. Create the Facilitator Spreadsheet

Create a separate Google spreadsheet that will contain the consolidated responses.

For example:

```text
Expert Elicitation - Facilitator
```

The facilitator spreadsheet can contain separate staging worksheets such as:

```text
P1 Responses
P2 Responses
P3 Responses
P4 Responses
P5 Responses
P6 Responses
Consolidated Responses
```

The participant-specific staging worksheets retrieve data from the separate participant spreadsheet files.

The `Consolidated Responses` worksheet then combines the imported participant responses into a single table that can be read by the Shiny application.

Only the facilitator and other individuals who require access to the complete response dataset should be given access to this spreadsheet.

---

# 6. Link Participant Spreadsheets Using IMPORTRANGE

Google Sheets provides the `IMPORTRANGE()` function for importing data from another Google spreadsheet.

The general syntax is:

```text
=IMPORTRANGE("spreadsheet_url", "sheet_name!range")
```

For example, the `P1 Responses` worksheet in the facilitator spreadsheet could contain:

```text
=IMPORTRANGE(
  "https://docs.google.com/spreadsheets/d/PARTICIPANT_SPREADSHEET_ID/edit",
  "Responses!A2:G100"
)
```

Replace:

```text
PARTICIPANT_SPREADSHEET_ID
```

with the URL or identifier for the appropriate participant spreadsheet.

Replace:

```text
Responses!A2:G100
```

with the worksheet name and cell range containing that participant's responses.

Repeat this process for each participant.

For example:

```text
P1 Responses ──► P1 spreadsheet
P2 Responses ──► P2 spreadsheet
P3 Responses ──► P3 spreadsheet
P4 Responses ──► P4 spreadsheet
P5 Responses ──► P5 spreadsheet
P6 Responses ──► P6 spreadsheet
```

## First-Time Access

The first time a facilitator spreadsheet uses `IMPORTRANGE()` to retrieve data from a participant spreadsheet, Google may display a message asking the user to connect the spreadsheets.

The facilitator may need to select:

```text
Allow access
```

before the imported data appear.

The Google account establishing the connection must have appropriate access to the source participant spreadsheet.

---

# 7. Consolidate the Participant Responses

After importing the participant responses, combine them into a single facilitator table.

The resulting table should have one row for each submitted participant response.

For example:

```text
Question | Round | Participant | LPP  | BGP  | HPP  | AC  | Notes
1        | 1     | P1          | 0.20 | 0.40 | 0.60 | 70  | 
1        | 1     | P2          | 0.30 | 0.50 | 0.75 | 65  | Less certain
1        | 1     | P3          | 0.25 | 0.45 | 0.65 | 80  | 
1        | 2     | P1          | 0.25 | 0.50 | 0.70 | 85  |
1        | 2     | P2          | 0.35 | 0.55 | 0.75 | 75  | Received more info
1        | 2     | P3          | 0.30 | 0.50 | 0.70 | 85  |
```

This consolidated table is the dataset that should be retrieved by the Shiny application.

Participants are not required to respond to every question or participate in every elicitation round. The number of responses can therefore differ among questions and rounds.

---

# 8. Use Google Sheets in the Shiny Application

After Google authentication and the facilitator spreadsheet have been configured, the Shiny application can retrieve the consolidated expert responses directly from Google Sheets.

Before retrieving data, confirm that the participant responses have populated the `Facilitator` worksheet of the facilitator Google spreadsheet.

## Retrieve Expert Responses

1. Run:

```text
App Version 2/Run-Application.R
```

2. In the application sidebar, select:

```text
Google Sheets
```

as the data source.

3. Select:

```text
Get Latest Expert Responses
```

The application retrieves the current contents of the `Facilitator` worksheet from the configured facilitator Google spreadsheet.

4. Open **Column Mapping** and confirm that the application has correctly identified the relevant columns:

```text
Question
Participant
Lowest plausible estimate
Best-guess estimate
Highest plausible estimate
Assessment Confidence
Round
```

The `Notes` column may be retained in the source data but is not required for the quantitative analysis.

5. Adjust the Column Mapping settings if any columns were not detected correctly.

6. Under **Questions**, select the question or questions to display.

7. Select:

```text
Run / Refresh Analysis
```

The application validates the retrieved responses before performing the analysis. If invalid responses are detected, the affected Question, Round, and response row are reported so that the source data can be corrected.

## Refresh Responses During an Elicitation

During an active elicitation exercise, participants can continue entering or revising responses in their individual Google spreadsheets.

After participants submit new responses:

1. Confirm that the new responses have propagated to the `Facilitator` worksheet in the facilitator Google spreadsheet.

2. In the Shiny application, select:

```text
Get Latest Expert Responses
```

again to retrieve the updated data.

3. Select:

```text
Run / Refresh Analysis
```

to update the analysis using the newly retrieved responses.

These two actions perform different functions:

```text
Get Latest Expert Responses
          ↓
Retrieves the current data from Google Sheets

Run / Refresh Analysis
          ↓
Validates and analyzes the data currently loaded in the application
```

Selecting **Run / Refresh Analysis** does not itself retrieve newly entered responses from Google Sheets. When participant responses have changed, use **Get Latest Expert Responses** first and then rerun the analysis.

## Two-Round Elicitation Workflow

For a two-round elicitation, a typical workflow is:

```text
Participants enter Round 1 responses
                ↓
Participant spreadsheets update
                ↓
Facilitator spreadsheet updates
                ↓
Confirm responses in the Facilitator worksheet
                ↓
Get Latest Expert Responses
                ↓
Run / Refresh Analysis
                ↓
Review aggregated Round 1 results
                ↓
Structured discussion
                ↓
Participants independently enter Round 2 responses
                ↓
Facilitator spreadsheet updates
                ↓
Confirm Round 2 responses in the Facilitator worksheet
                ↓
Get Latest Expert Responses
                ↓
Run / Refresh Analysis
                ↓
Compare Round 1 and Round 2 results
```
