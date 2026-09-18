import { expect } from 'chai';
import { BlobApiResponse, parseContentDispositionFilename } from '@swagger/typescript-fetch-petstore';

describe('parseContentDispositionFilename', () => {

    function nameOf(contentDisposition?: string): string | undefined {
        const headers = new Headers();
        if (contentDisposition !== undefined) {
            headers.set('Content-Disposition', contentDisposition);
        }
        return parseContentDispositionFilename(headers);
    }

    it('should return undefined without a header or without a filename', () => {
        expect(nameOf()).to.be.undefined;
        expect(nameOf('inline')).to.be.undefined;
        expect(nameOf('attachment; name="field"')).to.be.undefined;
    });

    it('should read a token and a quoted-string filename', () => {
        expect(nameOf('attachment; filename=report.pdf')).to.equal('report.pdf');
        expect(nameOf('attachment; filename="report 2024.pdf"')).to.equal('report 2024.pdf');
        expect(nameOf('attachment;filename="tight.pdf"')).to.equal('tight.pdf');
    });

    it('should keep semicolons and escaped quotes inside a quoted-string', () => {
        expect(nameOf('attachment; filename="report;2024.pdf"; size=12')).to.equal('report;2024.pdf');
        expect(nameOf('attachment; filename="say \\"hi\\".txt"')).to.equal('say "hi".txt');
    });

    it('should prefer the RFC 5987 encoded form and decode it', () => {
        expect(nameOf("attachment; filename=\"fallback.pdf\"; filename*=UTF-8''r%C3%A9sum%C3%A9.pdf"))
            .to.equal('résumé.pdf');
        expect(nameOf("attachment; filename*=utf-8'en'plain.pdf")).to.equal('plain.pdf');
    });

    it('should fall back to the plain form when the encoded one is malformed', () => {
        expect(nameOf("attachment; filename*=UTF-8''%E0%A4%A; filename=\"fallback.pdf\"")).to.equal('fallback.pdf');
    });

    it('should match parameter names case-insensitively', () => {
        expect(nameOf('Attachment; Filename="mixed.pdf"')).to.equal('mixed.pdf');
        expect(nameOf("attachment; FILENAME*=utf-8''upper.pdf")).to.equal('upper.pdf');
    });

    it('should only match filename as a parameter name', () => {
        expect(nameOf('attachment; xfilename="nope.pdf"')).to.be.undefined;
        expect(nameOf('attachment; name="filename*=UTF-8\'\'nope.pdf"; filename="real.pdf"')).to.equal('real.pdf');
    });

    it('should not split parameters on a semicolon inside a quoted-string', () => {
        expect(nameOf('attachment; name="a; filename*=UTF-8\'\'evil.pdf"; filename="real.pdf"')).to.equal('real.pdf');
        expect(nameOf('attachment; name="y; filename=evil"; filename="real.pdf"')).to.equal('real.pdf');
    });

    it('should accept a quoted encoded form and ignore a charset other than UTF-8', () => {
        expect(nameOf("attachment; filename*=\"UTF-8''quoted.pdf\"")).to.equal('quoted.pdf');
        expect(nameOf("attachment; filename*=iso-8859-1''latin.pdf; filename=\"plain.pdf\"")).to.equal('plain.pdf');
    });

    it('should keep the first occurrence of a repeated parameter', () => {
        expect(nameOf('attachment; filename="first.pdf"; filename="second.pdf"')).to.equal('first.pdf');
    });

    it('should drop any directory part of the name', () => {
        expect(nameOf('attachment; filename="../../etc/passwd"')).to.equal('passwd');
        expect(nameOf("attachment; filename*=UTF-8''..%2F..%2Fetc%2Fpasswd")).to.equal('passwd');
        expect(nameOf('attachment; filename=C:\\Users\\me\\report.pdf')).to.equal('report.pdf');
        expect(nameOf('attachment; filename=".."')).to.be.undefined;
        expect(nameOf('attachment; filename="/"')).to.be.undefined;
        expect(nameOf('attachment; filename=""')).to.be.undefined;
    });
});

describe('BlobApiResponse', () => {

    it('should name the file after the Content-Disposition header', async () => {
        const response = new Response('content', {
            headers: { 'Content-Type': 'text/plain', 'Content-Disposition': 'attachment; filename="named.txt"' },
        });
        const file = await new BlobApiResponse(response).value();
        expect(file).to.be.an.instanceOf(Blob);
        expect(file.name).to.equal('named.txt');
        expect(file.type).to.equal('text/plain');
        expect(await file.text()).to.equal('content');
    });

    it('should return an unnamed file without the header', async () => {
        const file = await new BlobApiResponse(new Response('content')).value();
        expect(file.name).to.equal('');
    });

    it('should keep returning the bare Blob where File is not a global (Node.js before 20)', async () => {
        const globals = global as any;
        const nativeFile = globals.File;
        globals.File = undefined;
        try {
            const value = await new BlobApiResponse(new Response('content')).value();
            expect(value).to.be.an.instanceOf(Blob);
            expect((value as any).name).to.be.undefined;
            expect(await value.text()).to.equal('content');
        } finally {
            globals.File = nativeFile;
        }
    });
});
