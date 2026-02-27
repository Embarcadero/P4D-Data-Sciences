import json

# Load the repositories from the text file
with open('depends.txt') as f:
    repos = [line.strip().split(',') for line in f]

# Convert to CycloneDX format
components = []
for repo in repos:
    url, path = repo
    name = url.split('/')[-1]  # Get the repository name
    components.append({
        "type": "library",
        "name": name,
        "version": "latest",
        "purl": f"pkg:github/{'/'.join(url.split('/')[-2:])}"
    })

cyclonedx_bom = {
    "bomFormat": "CycloneDX",
    "specVersion": "1.4",
    "version": 1,
    "components": components
}

# Write to a CycloneDX JSON file
with open('bom.json', 'w') as f:
    json.dump(cyclonedx_bom, f, indent=2)

print("CycloneDX SBOM written to bom.json")
