#!/bin/bash
docker run -it --rm \
  -v "$(pwd)":/workspace \
  -w /workspace \
  zupermind/mcdp:2025 \
  bash
