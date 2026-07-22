#!/usr/bin/env python3
# 00-extract_auctions.py
# Pull USD prices from a folder of CARB/WCI Joint Auction Summary Results Report PDFs.
# For each report, Table 3 gives every statistic in four columns:
#   Current USD | Current CAD | Advance USD | Advance CAD
# We keep the two USD columns (current = spot, advance = 3-year-forward).
#
# Scope: this parser targets the JOINT CA-QC report layout (four price columns).
# California-only reports before the November 2014 joint auction have two columns
# and will not match, which is one reason the paper's primary-market frame starts
# 2015Q2.
#
# Usage:  pip install pdfplumber
#         python3 00-extract_auctions.py /path/to/folder_of_pdfs  auctions.csv
#
# TWO CORRECTNESS FIXES relative to the original, both affecting numbers the paper
# reports directly:
#
#   [1] undersold flag. The original compared settlement to reserve with `==` on
#       values that are set to the EMPTY STRING when the Table 3 regex misses. If
#       both fields failed to parse, `"" == ""` is True and no exception is raised,
#       so the auction was flagged undersold on a parse failure. Table 7's count of
#       nine exact-reserve clearings is the paper's anchor empirical claim; a false
#       positive there is not survivable. Both operands must now be numeric.
#
#   [2] auction date. The original took the FIRST "on <Month> <D>, <YYYY>" anywhere
#       in the document. A publication or issue date appearing earlier in the text
#       silently wins, and a wrong auction date corrupts the join to positioning
#       and to the daily price series. The filename encodes month and year
#       (nc-aug_2015_..., aug_2013.pdf), so the parsed date is now cross-checked
#       against it and a mismatch is reported rather than absorbed.

import sys, os, re, csv, glob
import pdfplumber

LABELS = ["Auction Reserve Price", "Settlement Price", "Maximum Price",
          "Minimum Price", "Mean Price", "Median Price", "Median Allowance Price"]

MONTHS = {m: i for i, m in enumerate(
    ["January","February","March","April","May","June","July","August",
     "September","October","November","December"], 1)}

ABBR = {"jan":1, "feb":2, "mar":3, "apr":4, "may":5, "jun":6,
        "jul":7, "aug":8, "sep":9, "oct":10, "nov":11, "dec":12}


def date_from_filename(path):
    """(year, month) implied by the filename, or None."""
    b = os.path.basename(path).lower()
    m = re.search(r"(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*[_\-]?(20\d{2})", b)
    if m:
        return int(m.group(2)), ABBR[m.group(1)]
    return None


def parse_report(path):
    with pdfplumber.open(path) as pdf:
        full = "\n".join((p.extract_text() or "") for p in pdf.pages)

    rec = {"file": os.path.basename(path), "date_flag": ""}

    # [2] all candidate dates, then pick the one consistent with the filename.
    cands = []
    for m in re.finditer(r"on\s+([A-Z][a-z]+)\s+(\d{1,2}),\s*(\d{4})", full):
        if m.group(1) in MONTHS:
            cands.append((int(m.group(3)), MONTHS[m.group(1)], int(m.group(2))))
    fn = date_from_filename(path)
    chosen = None
    if fn and cands:
        hits = [c for c in cands if (c[0], c[1]) == fn]
        if hits:
            chosen = hits[0]
            if hits[0] != cands[0]:
                rec["date_flag"] = "first-match-overridden"
        else:
            chosen = cands[0]
            rec["date_flag"] = f"FILENAME-MISMATCH(expected {fn[0]}-{fn[1]:02d})"
    elif cands:
        chosen = cands[0]
        rec["date_flag"] = "no-filename-check"
    rec["date"] = f"{chosen[0]}-{chosen[1]:02d}-{chosen[2]:02d}" if chosen else ""

    m = re.search(r"Current Auction of (\d{4}) vintage", full)
    rec["current_vintage"] = m.group(1) if m else ""
    m = re.search(r"Advance Auction of\s+(\d{4})\s+vintage", full, re.S)
    rec["advance_vintage"] = m.group(1) if m else (
        str(int(rec["current_vintage"]) + 3) if rec["current_vintage"] else "")
    rec["undersold"] = ""

    for lab in LABELS:
        m = re.search(re.escape(lab) + r"\s+\$([\d.,]+)\s+\$[\d.,]+\s+\$([\d.,]+)\s+\$[\d.,]+", full)
        key = lab.lower().replace(" ", "_")
        if m:
            rec[key + "_cur"] = float(m.group(1).replace(",", ""))
            rec[key + "_adv"] = float(m.group(2).replace(",", ""))
        else:
            rec[key + "_cur"] = rec[key + "_adv"] = ""

    # [1] undersold: settlement == reserve to the cent, on numeric values only.
    def both_num(a, b):
        return isinstance(a, float) and isinstance(b, float)

    if both_num(rec["settlement_price_cur"], rec["auction_reserve_price_cur"]) \
       and abs(rec["settlement_price_cur"] - rec["auction_reserve_price_cur"]) < 0.005:
        rec["undersold"] += "current "
    if both_num(rec["settlement_price_adv"], rec["auction_reserve_price_adv"]) \
       and abs(rec["settlement_price_adv"] - rec["auction_reserve_price_adv"]) < 0.005:
        rec["undersold"] += "advance"
    if not both_num(rec["settlement_price_cur"], rec["auction_reserve_price_cur"]):
        rec["undersold"] = "PARSE-FAIL"
    return rec


def main():
    folder = sys.argv[1] if len(sys.argv) > 1 else "."
    out    = sys.argv[2] if len(sys.argv) > 2 else "auctions.csv"
    paths  = sorted(glob.glob(os.path.join(folder, "*.pdf")))
    if not paths:
        print("No PDFs found in", folder); return

    recs = []
    for p in paths:
        try:
            recs.append(parse_report(p))
        except Exception as e:
            print(f"  !! failed on {os.path.basename(p)}: {e}")
    recs.sort(key=lambda r: r.get("date", ""))

    cols = (["file", "date", "date_flag", "current_vintage", "advance_vintage", "undersold"] +
            [l.lower().replace(" ", "_") + s for l in LABELS for s in ("_cur", "_adv")])
    with open(out, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=cols); w.writeheader()
        for r in recs: w.writerow(r)

    print(f"\n{'date':12s} {'curV':5s} {'advV':5s} {'settle_cur':>11s} {'settle_adv':>11s} "
          f"{'fwd_slope/yr':>13s}  undersold / flag")
    for r in recs:
        sc, sa = r.get("settlement_price_cur"), r.get("settlement_price_adv")
        slope = f"{((sa/sc)**(1/3)-1)*100:6.2f}%" if (isinstance(sc, float) and isinstance(sa, float)) else "   n/a"
        print(f"{r.get('date',''):12s} {r.get('current_vintage',''):5s} "
              f"{r.get('advance_vintage',''):5s} {sc!s:>11} {sa!s:>11} {slope:>13}  "
              f"{r.get('undersold','')} {r.get('date_flag','')}")

    n_under = sum(1 for r in recs if r["undersold"].startswith("current"))
    n_fail  = sum(1 for r in recs if r["undersold"] == "PARSE-FAIL")
    n_flag  = sum(1 for r in recs if r["date_flag"].startswith("FILENAME-MISMATCH"))
    print(f"\nWrote {len(recs)} auctions to {out}.")
    print(f"  current-leg exact-reserve clearings: {n_under}   [paper Table 7: 9 over 2015Q2-2026Q1]")
    print(f"  Table 3 parse failures: {n_fail}    date/filename mismatches: {n_flag}")
    if n_fail or n_flag:
        print("  -> resolve these before use; they are silent corruption in the original parser.")


if __name__ == "__main__":
    main()
