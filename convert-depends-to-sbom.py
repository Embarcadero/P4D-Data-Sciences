import json
import os
import configparser
import subprocess


def parse_gitmodules(directory):
    """Return list of (submodule_path, url) from a .gitmodules file."""
    gitmodules_path = os.path.join(directory, '.gitmodules')
    if not os.path.exists(gitmodules_path):
        return []

    config = configparser.ConfigParser()
    config.read(gitmodules_path)

    submodules = []
    for section in config.sections():
        if section.startswith('submodule '):
            sub_path = config[section].get('path')
            url = config[section].get('url')
            if sub_path and url:
                submodules.append((sub_path, url))
    return submodules


def collect_submodules(directory, prefix=''):
    """Recursively collect all submodules as (relative_path, url) tuples."""
    result = []
    for sub_path, url in parse_gitmodules(directory):
        full_path = os.path.join(prefix, sub_path) if prefix else sub_path
        result.append((full_path, url))
        result.extend(collect_submodules(os.path.join(directory, sub_path), full_path))
    return result


def get_commit_hashes():
    """Return {submodule_path: commit_hash} from git submodule status --recursive."""
    proc = subprocess.run(
        ['git', 'submodule', 'status', '--recursive'],
        capture_output=True, text=True
    )
    hashes = {}
    for line in proc.stdout.strip().splitlines():
        parts = line.strip().split()
        if len(parts) >= 2:
            commit = parts[0].lstrip('+-')
            path = parts[1].replace('\\', '/')
            hashes[path] = commit
    return hashes


root_dir = os.path.dirname(os.path.abspath(__file__))
submodules = collect_submodules(root_dir)
commit_hashes = get_commit_hashes()

components = []
for path, url in submodules:
    name = url.rstrip('/').split('/')[-1]
    org_repo = '/'.join(url.rstrip('/').split('/')[-2:])
    norm_path = path.replace('\\', '/')
    commit = commit_hashes.get(norm_path, 'unknown')
    components.append({
        "type": "library",
        "name": name,
        "version": commit,
        "purl": f"pkg:github/{org_repo}@{commit}"
    })

cyclonedx_bom = {
    "bomFormat": "CycloneDX",
    "specVersion": "1.4",
    "version": 1,
    "components": components
}

with open('bom.json', 'w') as f:
    json.dump(cyclonedx_bom, f, indent=2)

print("CycloneDX SBOM written to bom.json")
