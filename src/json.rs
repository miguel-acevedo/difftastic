use serde::Serialize;
use std::collections::HashMap;

use crate::{
    context::all_matched_lines_filled,
    hunks::{matched_lines_for_hunk, Hunk},
    lines::LineNumber,
    side_by_side::lines_with_novel,
    syntax::{AtomKind, MatchKind, MatchedPos, TokenKind as SyntaxTokenKind},
};

#[derive(Debug, Serialize)]
#[serde(rename_all = "lowercase")]
enum Status {
    Changed,
    Created,
    Deleted,
}

#[derive(Debug, Serialize)]
struct File<'f> {
    language: &'f str,
    path: &'f str,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    chunks: Vec<Vec<Line<'f>>>,
    status: Status,
}

impl<'f> File<'f> {
    fn with_sections(language: &'f str, path: &'f str, chunks: Vec<Vec<Line<'f>>>) -> File<'f> {
        File {
            language,
            path,
            chunks,
            status: Status::Changed,
        }
    }

    fn with_status(language: &'f str, path: &'f str, status: Status) -> File<'f> {
        File {
            language,
            path,
            chunks: Vec::new(),
            status,
        }
    }
}

#[derive(Debug, Serialize)]
struct Line<'l> {
    #[serde(skip_serializing_if = "Option::is_none")]
    lhs: Option<Side<'l>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    rhs: Option<Side<'l>>,
}

impl<'l> Line<'l> {
    fn new(lhs_number: Option<usize>, rhs_number: Option<usize>) -> Line<'l> {
        Line {
            lhs: lhs_number.map(Side::new),
            rhs: rhs_number.map(Side::new),
        }
    }
}

#[derive(Debug, Serialize)]
struct Side<'s> {
    number: usize,
    changes: Vec<Change<'s>>,
}

impl<'s> Side<'s> {
    fn new(number: usize) -> Side<'s> {
        Side {
            number,
            changes: Vec::new(),
        }
    }
}

#[derive(Debug, Serialize)]
struct Change<'c> {
    start: usize,
    end: usize,
    content: &'c str,
    kind: TokenKind,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "lowercase")]
// TODO: use syntax::TokenKind and syntax::AtomKind instead of this merged enum, blocked by https://github.com/serde-rs/serde/issues/1402
enum TokenKind {
    Delimiter,
    Normal,
    String,
    Type,
    Comment,
    Keyword,
}

impl From<SyntaxTokenKind> for TokenKind {
    fn from(kind: SyntaxTokenKind) -> Self {
        match kind {
            SyntaxTokenKind::Delimiter => TokenKind::Delimiter,
            SyntaxTokenKind::Atom(a) => match a {
                AtomKind::String => TokenKind::String,
                AtomKind::Keyword => TokenKind::Keyword,
                AtomKind::Comment => TokenKind::Comment,
                AtomKind::Type => TokenKind::Type,
                AtomKind::Normal => TokenKind::Normal,
            },
        }
    }
}

pub fn print(
    hunks: &[Hunk],
    display_path: &str,
    lang_name: &str,
    lhs_src: &str,
    rhs_src: &str,
    lhs_mps: &[MatchedPos],
    rhs_mps: &[MatchedPos],
) {
    if lhs_src.is_empty() {
        println!(
            "{}",
            serde_json::to_string(&File::with_status(lang_name, display_path, Status::Created))
                .expect("failed to serialize created file")
        );
        return;
    }
    if rhs_src.is_empty() {
        println!(
            "{}",
            serde_json::to_string(&File::with_status(lang_name, display_path, Status::Deleted))
                .expect("failed to serialize deleted file")
        );
        return;
    }

    let lhs_lines = lhs_src.split('\n').collect::<Vec<&str>>();
    let rhs_lines = rhs_src.split('\n').collect::<Vec<&str>>();

    let (lhs_lines_with_novel, rhs_lines_with_novel) = lines_with_novel(lhs_mps, rhs_mps);
    let matched_lines = all_matched_lines_filled(lhs_mps, rhs_mps);

    let mut chunks = Vec::with_capacity(hunks.len());
    for hunk in hunks.iter() {
        let mut lines =
            HashMap::<(Option<usize>, Option<usize>), Line>::with_capacity(hunk.lines.len());

        let aligned_lines = matched_lines_for_hunk(&matched_lines, hunk);

        for (lhs_line_num, rhs_line_num) in aligned_lines {
            if !lhs_lines_with_novel.contains(&lhs_line_num.unwrap_or(LineNumber(0)))
                && !rhs_lines_with_novel.contains(&rhs_line_num.unwrap_or(LineNumber(0)))
            {
                continue;
            }

            let line = lines
                .entry((lhs_line_num.map(|l| l.0), rhs_line_num.map(|l| l.0)))
                .or_insert_with(|| Line::new(lhs_line_num.map(|l| l.0), rhs_line_num.map(|l| l.0)));

            if let Some(line_num) = lhs_line_num {
                add_changes_to_side(line.lhs.as_mut().unwrap(), line_num, &lhs_lines, lhs_mps);
            }
            if let Some(line_num) = rhs_line_num {
                add_changes_to_side(line.rhs.as_mut().unwrap(), line_num, &rhs_lines, rhs_mps);
            }
        }

        chunks.push(lines.into_values().collect());
    }

    println!(
        "{}",
        serde_json::to_string(&File::with_sections(lang_name, display_path, chunks))
            .expect("failed to serialize file")
    );
}

fn matches_for_line(matches: &[MatchedPos], line_num: LineNumber) -> Vec<&MatchedPos> {
    matches
        .into_iter()
        .filter(|m| m.pos.line == line_num)
        .filter(|m| m.kind.is_change())
        .collect()
}

fn add_changes_to_side<'s>(
    side: &mut Side<'s>,
    line_num: LineNumber,
    src_lines: &[&'s str],
    all_matches: &[MatchedPos],
) {
    let src_line = src_lines[line_num.0];

    let matches = matches_for_line(all_matches, line_num);
    for m in matches {
        side.changes.push(Change {
            start: m.pos.start_col,
            end: m.pos.end_col,
            content: &src_line[m.pos.start_col..m.pos.end_col],
            kind: match m.kind {
                MatchKind::UnchangedToken { highlight, .. } => highlight,
                MatchKind::Novel { highlight, .. } => highlight,
                MatchKind::NovelLinePart { highlight, .. } => highlight,
                MatchKind::NovelWord { highlight, .. } => highlight,
            }
            .into(),
        })
    }
}
