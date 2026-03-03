#!/usr/bin/env python3
"""
Create GitHub issues for performance optimizations.

This script reads the markdown files in /tmp/performance-issues/ and creates
GitHub issues using the GitHub API.

Requirements:
- GITHUB_TOKEN environment variable must be set with appropriate permissions
- Repository: Open-Systems-Pharmacology/OSPSuite.ReportingFramework

Usage:
    python3 create_github_issues.py [--dry-run]
"""

import os
import sys
import json
import glob
import argparse
from pathlib import Path
try:
    import requests
except ImportError:
    print("Warning: requests library not available, using urllib instead")
    import urllib.request
    import urllib.error
    requests = None


def create_issue_with_requests(title, body, labels, token, repo_owner, repo_name, dry_run=False):
    """Create issue using requests library"""
    url = f"https://api.github.com/repos/{repo_owner}/{repo_name}/issues"

    headers = {
        "Authorization": f"Bearer {token}",
        "Accept": "application/vnd.github.v3+json",
        "Content-Type": "application/json"
    }

    data = {
        "title": title,
        "body": body,
        "labels": labels
    }

    if dry_run:
        print(f"\n{'='*80}")
        print(f"DRY RUN - Would create issue:")
        print(f"Title: {title}")
        print(f"Labels: {', '.join(labels)}")
        print(f"Body length: {len(body)} characters")
        print(f"{'='*80}\n")
        return {"number": "DRY-RUN", "html_url": "dry-run-mode"}

    response = requests.post(url, headers=headers, json=data)

    if response.status_code == 201:
        result = response.json()
        return result
    else:
        print(f"Error: {response.status_code} {response.reason}")
        print(response.text)
        return None


def create_issue_with_urllib(title, body, labels, token, repo_owner, repo_name, dry_run=False):
    """Create issue using urllib (fallback)"""
    url = f"https://api.github.com/repos/{repo_owner}/{repo_name}/issues"

    headers = {
        "Authorization": f"Bearer {token}",
        "Accept": "application/vnd.github.v3+json",
        "Content-Type": "application/json"
    }

    data = {
        "title": title,
        "body": body,
        "labels": labels
    }

    if dry_run:
        print(f"\n{'='*80}")
        print(f"DRY RUN - Would create issue:")
        print(f"Title: {title}")
        print(f"Labels: {', '.join(labels)}")
        print(f"Body length: {len(body)} characters")
        print(f"{'='*80}\n")
        return {"number": "DRY-RUN", "html_url": "dry-run-mode"}

    request = urllib.request.Request(
        url,
        data=json.dumps(data).encode('utf-8'),
        headers=headers,
        method='POST'
    )

    try:
        with urllib.request.urlopen(request) as response:
            result = json.loads(response.read().decode('utf-8'))
            return result
    except urllib.error.HTTPError as e:
        print(f"Error: {e.code} {e.reason}")
        print(e.read().decode('utf-8'))
        return None


def parse_markdown_file(filepath):
    """Parse markdown file to extract title and body"""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    lines = content.strip().split('\n')

    # Extract title (first line, remove # prefix)
    title = lines[0].lstrip('# ').strip()

    # Body is everything after the first line
    body = '\n'.join(lines[1:]).strip()

    return title, body


def extract_priority_from_filename(filename):
    """Extract priority number from filename like '01-issue.md' -> 1"""
    try:
        return int(filename.split('-')[0])
    except (ValueError, IndexError):
        return 999


def main():
    parser = argparse.ArgumentParser(description='Create GitHub issues for performance optimizations')
    parser.add_argument('--dry-run', action='store_true', help='Show what would be created without actually creating issues')
    parser.add_argument('--repo-owner', default='Open-Systems-Pharmacology', help='Repository owner')
    parser.add_argument('--repo-name', default='OSPSuite.ReportingFramework', help='Repository name')
    parser.add_argument('--issues-dir', default='/tmp/performance-issues', help='Directory containing issue markdown files')
    args = parser.parse_args()

    # Get GitHub token
    token = os.environ.get('GITHUB_TOKEN')
    if not token:
        print("Error: GITHUB_TOKEN environment variable not set")
        sys.exit(1)

    # Find all markdown files except README
    issues_dir = Path(args.issues_dir)
    if not issues_dir.exists():
        print(f"Error: Issues directory not found: {issues_dir}")
        sys.exit(1)

    md_files = sorted(glob.glob(str(issues_dir / "*.md")))
    md_files = [f for f in md_files if '00-README' not in f]

    if not md_files:
        print(f"Error: No issue files found in {issues_dir}")
        sys.exit(1)

    # Sort by filename (priority)
    md_files.sort(key=lambda x: extract_priority_from_filename(Path(x).name))

    print(f"Found {len(md_files)} issue files to process")
    print(f"Repository: {args.repo_owner}/{args.repo_name}")
    if args.dry_run:
        print("DRY RUN MODE - No issues will be created")
    print()

    # Choose the appropriate function
    create_issue = create_issue_with_requests if requests else create_issue_with_urllib

    # Create issues
    created_issues = []
    failed_issues = []

    for i, filepath in enumerate(md_files, 1):
        filename = Path(filepath).name
        print(f"[{i}/{len(md_files)}] Processing {filename}...")

        try:
            title, body = parse_markdown_file(filepath)

            # Add footer to body
            body += "\n\n---\n*This issue was automatically generated from performance analysis of the codebase.*"

            # Create issue with appropriate labels
            labels = ["performance", "enhancement"]

            result = create_issue(
                title=title,
                body=body,
                labels=labels,
                token=token,
                repo_owner=args.repo_owner,
                repo_name=args.repo_name,
                dry_run=args.dry_run
            )

            if result:
                issue_num = result.get('number')
                issue_url = result.get('html_url')
                print(f"  ✓ Created issue #{issue_num}: {issue_url}")
                created_issues.append((filename, issue_num, issue_url))
            else:
                print(f"  ✗ Failed to create issue from {filename}")
                failed_issues.append(filename)

        except Exception as e:
            print(f"  ✗ Error processing {filename}: {e}")
            failed_issues.append(filename)

    # Summary
    print(f"\n{'='*80}")
    print("SUMMARY")
    print(f"{'='*80}")
    print(f"Successfully created: {len(created_issues)} issues")
    if failed_issues:
        print(f"Failed: {len(failed_issues)} issues")
        for filename in failed_issues:
            print(f"  - {filename}")

    if created_issues:
        print("\nCreated issues:")
        for filename, issue_num, issue_url in created_issues:
            print(f"  #{issue_num}: {filename}")
            print(f"           {issue_url}")

    print(f"\n{'='*80}\n")

    if failed_issues:
        sys.exit(1)


if __name__ == "__main__":
    main()
