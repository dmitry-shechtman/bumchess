# bumchess
Branchless Unmake/Make Chess Move Generator

## Usage
`perft [<fen>] [<depth> [<result>]] [-d<divide>] [-p<threads>] [-l]`

- `<fen>`       - Specifies the position in FEN format (castlings and e.p. square are optional)
- `<depth>`     - Specifies the depth
- `<result>`    - Specifies the expected leaf count
- `-d<divide>`  - Divide from depth 1 to `<divide>`
- `-p<threads>` - Run in parallel `<threads>`
- `-l`          - Loop from depth 0 to `<depth>`

## Not Yet Implemented
* Castling

## Known Limits
* 1 king per side
* 4 knights per side
* 4 bishops per side
* 4 rooks per side
* 3 queens per side
* 8 pawns per side

## Thanks
* Harm Geert Muller
* Rasmus Althoff
* Daniel Inführ
* Sven Schüle
* Marcel Vanthoor

## License
Copyright (c) 2022 Dmitry Shechtman

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
